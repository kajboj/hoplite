adb = "/home/kajboj/code/adt/adt-bundle-linux-x86-20131030/sdk/platform-tools/adb"
regex = 's/\x0D\x0A/\x0A/g'
`#{adb} shell screencap -p | perl -pe '#{regex}' > screen.png`
load 'png_reader.rb'
output = `scheme --silent < hoplite.scm`
puts output

col, row = 600, 550

`./adb shell input tap #{col} #{row}`
