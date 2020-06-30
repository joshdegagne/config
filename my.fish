# PROMPT ---------------------------------------------------------------------------------
## parameters
set delim '<>'
set delim_colour white
set delim_width (string length $delim)
set colour_order magenta brmagenta blue cyan yellow
set time_format "%Y:%b:%d:%Z:%H:%M:%S"

function info_format -a terminal_width
  set info $argv[2..-1]
  function rec_info_format -a tw pos n first
    set rest $argv[5..-1] # everything that is not a named arg

    if test -n "$first" # end condition
      set len (string length $first)
      if test $pos -ne 0 # the first one doesn't have a delimiter
        set total (math $pos + $len + $delim_width)
      else
        set total (math $pos + $len)
      end
      if test $total -le $tw
        set npos $total
      else
        set npos $len
      end

      if test $npos -eq $len
        if test $pos -ne 0
          printf ' %.0s' (seq (math $tw - $pos)) # fill line with bg colour
          printf "\n"
        end
      else
        printf "%s" (set_color $delim_colour; echo $delim)
      end
      printf "%s" (set_color $colour_order[$n]; echo $first)

      rec_info_format $tw $npos (math $n + 1) $rest
    else
      printf ' %.0s' (seq (math $tw - $pos)) # fill line with bg colour
      printf "\n"
    end
  end

  set_color -b black
  rec_info_format $terminal_width 0 1 $info
  set_color normal
end

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

function fish_prompt
  printf "\n"
  ## info line(s)
  set terminal_width (tput cols)
  set cluster_name (kubectl config current-context)
  set t_git_branch (truncate (echo (git branch 2> /dev/null | grep "^\*" | cut -d \  -f 2)) 40)
  set date_time (date +$time_format)
  # these `info` will always be there
  set info (echo $date_time) (hostname) (whoami)
  # these `info` could be blank
  if test -n $cluster_name; set -a info (echo $cluster_name); end
  if test -n $t_git_branch; set -a info (echo $t_git_branch); end
  info_format $terminal_width $info

  ## pwd prompt line
  printf "%s%s" (set_color brred; prompt_pwd) (set_color normal; echo '|> ')
end

# PATH -----------------------------------------------------------------------------------
set -g fish_user_paths "/usr/local/sbin" $fish_user_paths

# TOOLS ----------------------------------------------------------------------------------
## os specific
switch (uname)
  case Darwin
    ### brew
    abbr brewup 'brew update ; brew upgrade ; brew cask outdated | xargs -I _ brew cask upgrade _'
    ### jenv
    set PATH '~/.jenv/bin' $PATH
    status --is-interactive; and source (jenv init -|psub)
end

## everywhere
### thefuck
thefuck --alias | source
