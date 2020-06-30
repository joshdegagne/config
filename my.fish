# prelude
function truncate -a str _max dots
  test -n "$_max" ; or set _max 10
  test -n "$dots" ; or set dots '...'
  set max (math $_max-(echo "$dots" | wc -m)+1)
  if test -z "$str" -o (echo "$str" | wc -m) -le "$max"
    echo "$str"
  else
    echo -e (echo "$str" | cut -c 1-$max) \b(echo $dots)
  end
end

function sum
  math (for i in (seq (count $argv))
    if test $i -gt 1; printf "+"; end
    printf "%s" $argv[$i]
  end)
end

function sum_list
  set nums $argv
  for i in (seq (count $nums)); sum $nums[1..$i]; end
end

# prompt
function fish_prompt
  ## info line(s)
  set terminal_width (tput cols)
  set delimiter '<>'
  set delimiter_colour white
  set delimiter_width (string length $delimiter)
  set colour_order magenta brmagenta blue cyan yellow
  set cluster_name (kubectl config current-context)
  set t_git_branch (truncate (echo (git branch 2> /dev/null | grep "^\*" | cut -d \  -f 2)) 40)
  set date_time (date +"%Y:%b:%d:%Z:%H:%M:%S")

  # these `info` will always be there
  set info (echo $date_time) (hostname) (whoami)
  # these `info` could be blank
  if test -n $cluster_name; set -a info (echo $cluster_name); end
  if test -n $t_git_branch; set -a info (echo $t_git_branch); end

  set info_widths (for i in $info; string length $i; end)
  set accum_widths (sum_list (for i in (seq (count $info_widths))
    if test $i -eq 1
      math $info_widths[$i]
    else
      math $info_widths[$i] + $delimiter_width
    end
  end))

  set_color -b black
  for i in (seq (count $info))
    set item $info[$i]
    set item_colour $colour_order[$i]
    set width_so_far $accum_widths[$i]
    if test $i -gt 1
      if test $width_so_far -gt $terminal_width
        printf "\n"
      else
        printf "%s" (set_color $delimiter_colour; echo $delimiter)
      end
    end
    printf "%s" (set_color $item_colour; echo $item)
  end

  ## pwd prompt line
  set_color normal
  printf "\n%s%s" (set_color brred; prompt_pwd) (set_color normal; echo '|> ')
end

# path
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths

# tools
## thefuck
thefuck --alias | source
## jenv
set PATH '~/.jenv/bin' $PATH
status --is-interactive; and source (jenv init -|psub)
