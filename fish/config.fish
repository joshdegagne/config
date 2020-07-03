# PROMPT ---------------------------------------------------------------------------------

## parameters
set delim '<>'
set delim_colour black
set delim_width (string length $delim)
set colour_order magenta brmagenta blue cyan green yellow brred
set time_format "%Y:%b:%d:%Z:%H:%M:%S"
set fish_prompt_pwd_raw_levels 2
set fish_prompt_pwd_dir_length 2

function fish_prompt
  # extra space is relaxing
  printf "\n"
  # gather info
  set terminal_width (tput cols)
  set cluster_name (kubectl config current-context)
  set t_git_branch (truncate 40 '...' (current_git_branch))
  set date_time (date +$time_format)
  # these `info` will always be there
  set info (echo $date_time) (hostname) (whoami)
  # these `info` could be blank
  if test -n $cluster_name; set -a info (echo $cluster_name); end
  if test -n $t_git_branch; set -a info (echo $t_git_branch); end

  # info line(s)
  info_format $terminal_width $info
  # pwd prompt line
  printf "%s%s" (set_color brred; prompt_pwd) (set_color normal; echo '|> ')
end

function prompt_pwd --description 'Print the current working directory, shortened to fit the prompt'
  set -l options 'h/help'
  argparse -n prompt_pwd --max-args=0 $options -- $argv
  or return

  if set -q _flag_help
    __fish_print_help prompt_pwd
    return 0
  end

  # This allows overriding fish_prompt_pwd_dir_length and fish_prompt_pwd_raw_levels from the outside (global or universal) without leaking them
  set -q fish_prompt_pwd_dir_length
  or set -l fish_prompt_pwd_dir_length 1
  set -q fish_prompt_pwd_raw_levels
  or set -l fish_prompt_pwd_raw_levels 1

  # Replace $HOME with "~"
  set realhome ~
  set -l tmp (string replace -r '^'"$realhome"'($|/)' '~$1' $PWD)

  if [ $fish_prompt_pwd_dir_length -eq 0 ]
    echo $tmp
  else
    # Shorten to at most $fish_prompt_pwd_dir_length characters per directory
    # and don't shorten at most $fish_prompt_pwd_raw_levels directories at the end
    set p (string split \/ $tmp)
    set n (count $p)
    for i in (seq $n)
      if test $i -le (math $n - $fish_prompt_pwd_raw_levels)
        printf "%s/" (string sub -l $fish_prompt_pwd_dir_length $p[$i])
      else if test $i -eq $n
        printf "%s" $p[$i]
      else
        printf "%s/" $p[$i]
      end
    end
  end
end

##### WIP
function __fish_winch_handler --on-signal SIGWINCH
__fish_cancel_commandline
commandline -C (count ) -f repaint >/dev/null 2>/dev/null
end
##### END WIP

function fish_title
  set -l command (echo $_)
  printf "%s" (prompt_pwd)
  if test $command != "fish"; printf " %s" $command; end
end

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

  set_color -b white
  rec_info_format $terminal_width 0 1 $info
  set_color normal
end

function truncate -a _max suffix str
  if test -n "$str" -a $_max -eq 0
    test -n "$_max" ; or set _max 10
    set max (math $_max-(echo "$suffix" | wc -m)+1)
    if test -z "$str" -o (echo "$str" | wc -m) -le "$max"
      echo "$str"
    else
      echo -e (echo "$str" | cut -c 1-$max) \b(echo $suffix)
    end
  else
    echo $str
  end
end

function current_git_branch
  git branch 2> /dev/null | grep "^\*" | cut -d \  -f 2
end

# FISH HIGHLIGHTING COLOURS --------------------------------------------------------------

## custom solarized light
set -U fish_color_normal normal
set -U fish_color_command 859900
set -U fish_color_redirection b58900
set -U fish_color_quote 2aa198
set -U fish_color_param 268bd2
set -U fish_color_comment 93a1a1
set -U fish_color_error d33682
set -U fish_color_search_match bryellow --background=white
set -U fish_color_history_current --bold
set -U fish_color_operator 00a6b2
set -U fish_color_escape 00a6b2
set -U fish_color_cwd green
set -U fish_color_cwd_root red
set -U fish_color_valid_path --underline
set -U fish_color_autosuggestion 657b83
set -U fish_color_user brgreen
set -U fish_color_host normal
set -U fish_color_cancel -r
set -U fish_pager_color_completion green
set -U fish_pager_color_description B3A06D
set -U fish_pager_color_prefix cyan --underline
set -U fish_pager_color_progress brwhite --background=cyan
set -U fish_color_match --background=brblue
set -U fish_color_end 6c71c4
set -U fish_color_selection white --bold --background=brblack

# PATH -----------------------------------------------------------------------------------

set -g fish_user_paths "/usr/local/sbin" $fish_user_paths

# TOOLS ----------------------------------------------------------------------------------

## os specific
switch (uname)
  case Darwin
    ### brew
    abbr brewup 'brew update ; brew upgrade ; brew cask upgrade (brew cask outdated --greedy --json | jq --raw-output \'.[] | select((.installed_versions == "latest" and .current_version == "latest")| not) | .name\' | xargs -I _ printf "%s " _)'
    ### jenv
    set -p PATH '~/.jenv/bin'
    status --is-interactive; and source (jenv init -|psub)
end

## everywhere
thefuck --alias | source

alias ftp 'ftp -i' # no interactive prompt

abbr cltree 'tree -aFI ".git|target"'      # for (cl)ojure projects
abbr emtree 'tree -aFI ".git|elpa|quelpa"' # for (em)acs configs

alias untargz 'tar -zxvf'

function targz -a filename --description 'tar something as the same name'
  tar -zcvf $filename.tar.gz $filename
end

function gzhead -a filename --description 'useful for peaking at csv.gz headers'
  gzip -cd $filename | head
end
