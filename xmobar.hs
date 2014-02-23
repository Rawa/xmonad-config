-- xmobar config used by David Göransson
-- Author: David Göransson
-- http://github.com/Rawa/xmonad-config

Config {
    bgColor = "#000000",
    fgColor = "#ffffff",
    position = Static { xpos = 0 , ypos = 0, width = 1600, height = 16 },
    lowerOnStart = True,
    commands = [
        Run Weather "ESGG" ["-t","<tempC>°C <skyCondition>","-L","64","-H","77","-n","#CEFFAC","-h","#FFB6B0","-l","#96CBFE"] 36000,
        Run Cpu ["-L","30","-H","60","--low", "green","--normal","orange","--high","red"] 10,
        Run Memory ["-t","Mem: <usedratio>%","-H","8192","-L","4096","-h","red","-l","green","-n","orange"] 10,
        Run Swap ["-t","Swap: <usedratio>%","-H","1024","-L","512","-h","red","-l","green","-n","orange"] 10,
        Run Wireless "wlp2s0" ["-t", "Wlan: <essid>/<quality>%","-L", "33", "-H", "66", "--low", "red","--normal", "orange", "--high", "green"] 10,
        Run Date "%a %b %_d %Y %H:%M:%S" "date" 10,
        Run Com "/bin/bash" ["/home/rawa/.xmonad/bin/getVolume"] "myvolume" 1,
        Run BatteryP["BAT0"] ["-t", "Batt: <left>%(<acstatus>)","-L","20","-H","50","-h","green","-n","orange","-l","red", "--", "-O", "<fc=green>+</fc>","-o", "<fc=red>-</fc>", "-f", "/sys/class/power_supply/AC0/online"] 10,
        Run StdinReader
    ],
    sepChar = "%",
    alignSep = "}{",
    template = "%StdinReader% }{ %cpu% %memory% %swap% %wlp2s0wi% %battery% Vol: %myvolume% | %ESGG% | <fc=#FFFFCC>%date%</fc> "
}
