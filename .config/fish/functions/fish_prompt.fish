set green (set_color green)
set magenta (set_color magenta)
set normal (set_color normal)
set red (set_color red)
set yellow (set_color yellow)

function fish_prompt
	set_color magenta 
	printf '%s' (prompt_pwd)

	set_color yellow
	fish_git_prompt $argv

	set_color magenta
	echo -n ' $ '
	# set_color yellow
end
