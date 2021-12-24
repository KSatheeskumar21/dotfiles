local appmenu = {}

appmenu.Accessories = {
    { 'AppImageLauncher Settings', 'AppImageLauncherSettings', '/usr/share/icons/hicolor/512x512/apps/AppImageLauncher.png' },
    { 'ArcoLinux Betterlockscreen', '/usr/local/bin/arcolinux-betterlockscreen' },
    { 'Arcolinux Tweak Tool', '/usr/local/bin/arcolinux-tweak-tool', '/usr/share/icons/hicolor/scalable/apps/arcolinux-tweak-tool.svg' },
    { 'Bulk Rename', 'thunar --bulk-rename', '/usr/share/icons/hicolor/128x128/apps/org.xfce.thunar.png' },
    { 'File Manager PCManFM', 'pcmanfm' },
    { 'Neovim', 'xterm -e nvim', '/usr/share/icons/hicolor/128x128/apps/nvim.png' },
    { 'Software Token', 'stoken-gui' },
    { 'Software Token (small)', 'stoken-gui --small' },
    { 'Thunar File Manager', 'thunar', '/usr/share/icons/hicolor/128x128/apps/org.xfce.thunar.png' },
    { 'Vim', 'xterm -e vim', '/usr/share/icons/hicolor/48x48/apps/gvim.png' },
    { 'Visual Studio Code', '/opt/visual-studio-code/code --no-sandbox --unity-launch' },
    { 'compton', 'compton', '/usr/share/icons/hicolor/scalable/apps/compton.svg' },
    { 'nitrogen', 'nitrogen', '/usr/share/icons/hicolor/128x128/apps/nitrogen.png' },
    { 'picom', 'picom' },
}

appmenu.Development = {
    { 'CMake', 'cmake-gui', '/usr/share/icons/hicolor/128x128/apps/CMakeSetup.png' },
    { 'Emacs', 'emacs', '/usr/share/icons/hicolor/128x128/apps/emacs.png' },
    { 'Icon Browser', 'yad-icon-browser', '/usr/share/icons/hicolor/96x96/apps/yad.png' },
    { 'Qt Assistant', 'assistant', '/usr/share/icons/hicolor/128x128/apps/assistant.png' },
    { 'Qt Designer', 'designer', '/usr/share/icons/hicolor/128x128/apps/QtProject-designer.png' },
    { 'Qt Linguist', 'linguist', '/usr/share/icons/hicolor/128x128/apps/linguist.png' },
    { 'Qt QDBusViewer', 'qdbusviewer', '/usr/share/icons/hicolor/128x128/apps/qdbusviewer.png' },
    { 'Visual Studio Code', '/opt/visual-studio-code/code --no-sandbox --unity-launch' },
}

appmenu.Internet = {
    { 'Avahi SSH Server Browser', '/usr/bin/bssh' },
    { 'Avahi VNC Server Browser', '/usr/bin/bvnc' },
    { 'Brave', 'brave', '/usr/share/icons/hicolor/128x128/apps/brave-desktop.png' },
    { 'Firefox', '/usr/lib/firefox/firefox', '/usr/share/icons/hicolor/128x128/apps/firefox.png' },
    { 'qutebrowser', 'qutebrowser --untrusted-args', '/usr/share/icons/hicolor/512x512/apps/qutebrowser.png' },
}

appmenu.Office = {
    { 'Zathura', 'zathura', '/usr/share/icons/hicolor/128x128/apps/org.pwmt.zathura.png' },
}

appmenu.MultiMedia = {
    { 'DeaDBeeF', 'deadbeef', '/usr/share/icons/hicolor/96x96/apps/deadbeef.png' },
    { 'Olivia', 'olivia', '/usr/share/icons/hicolor/512x512/apps/olivia.png' },
    { 'PulseAudio Volume Control', 'pavucontrol' },
    { 'Qt V4L2 test Utility', 'qv4l2', '/usr/share/icons/hicolor/64x64/apps/qv4l2.png' },
    { 'Qt V4L2 video capture utility', 'qvidcap', '/usr/share/icons/hicolor/64x64/apps/qvidcap.png' },
    { 'Spotify', 'spotify --uri=' },
    { 'VLC media player', '/usr/bin/vlc --started-from-file', '/usr/share/icons/hicolor/128x128/apps/vlc.png' },
    { 'Volume Icon', 'volumeicon' },
    { 'mpv Media Player', 'mpv --player-operation-mode=pseudo-gui --', '/usr/share/icons/hicolor/128x128/apps/mpv.png' },
}

appmenu.Settings = {
    { 'ARandR', 'arandr' },
    { 'Advanced Network Configuration', 'nm-connection-editor' },
    { 'AppImageLauncher Settings', 'AppImageLauncherSettings', '/usr/share/icons/hicolor/512x512/apps/AppImageLauncher.png' },
    { 'ArcoLinux Betterlockscreen', '/usr/local/bin/arcolinux-betterlockscreen' },
    { 'ArcoLinux Welcome', '/usr/local/bin/arcolinux-welcome-app' },
    { 'Arcolinux Tweak Tool', '/usr/local/bin/arcolinux-tweak-tool', '/usr/share/icons/hicolor/scalable/apps/arcolinux-tweak-tool.svg' },
    { 'Customize Look and Feel', 'lxappearance' },
    { 'Desktop Preferences', 'pcmanfm --desktop-pref' },
    { 'File Manager Settings', 'thunar-settings', '/usr/share/icons/hicolor/128x128/apps/org.xfce.thunar.png' },
    { 'PulseAudio Volume Control', 'pavucontrol' },
    { 'Removable Drives and Media', 'thunar-volman-settings', '/usr/share/icons/hicolor/128x128/apps/org.xfce.volman.png' },
    { 'Xfce Terminal Settings', 'xfce4-terminal --preferences', '/usr/share/icons/hicolor/128x128/apps/org.xfce.terminal-settings.png' },
    { 'YAD settings', 'yad-settings', '/usr/share/icons/hicolor/96x96/apps/yad.png' },
}

appmenu.System = {
    { 'Alacritty', 'alacritty' },
    { 'Avahi Zeroconf Browser', '/usr/bin/avahi-discover' },
    { 'Bulk Rename', 'thunar --bulk-rename', '/usr/share/icons/hicolor/128x128/apps/org.xfce.thunar.png' },
    { 'File Manager PCManFM', 'pcmanfm' },
    { 'GParted', '/usr/bin/gparted', '/usr/share/icons/hicolor/scalable/apps/gparted.svg' },
    { 'Hardware Locality lstopo', 'lstopo' },
    { 'Htop', 'xterm -e htop', '/usr/share/icons/hicolor/scalable/apps/htop.svg' },
    { 'Termite', 'termite' },
    { 'Thunar File Manager', 'thunar', '/usr/share/icons/hicolor/128x128/apps/org.xfce.thunar.png' },
    { 'UXTerm', 'uxterm' },
    { 'XTerm', 'xterm' },
    { 'Xfce Terminal', 'xfce4-terminal', '/usr/share/icons/hicolor/128x128/apps/org.xfce.terminal.png' },
    { 'fish', 'xterm -e fish' },
    { 'kitty', 'kitty', '/usr/share/icons/hicolor/scalable/apps/kitty.svg' },
    { 'ranger', 'xterm -e ranger' },
    { 'urxvt', 'urxvt' },
    { 'urxvt (client)', 'urxvtc' },
    { 'urxvt (tabbed)', 'urxvt-tabbed' },
}

appmenu.Appmenu = {
    { 'Accessories', appmenu.Accessories },
    { 'Development', appmenu.Development },
    { 'Internet', appmenu.Internet },
    { 'Office', appmenu.Office },
    { 'MultiMedia', appmenu.MultiMedia },
    { 'Settings', appmenu.Settings },
    { 'System', appmenu.System },
}

return appmenu