adb = "/home/kajboj/code/adt/adt-bundle-linux-x86-20131030/sdk/platform-tools/adb"
regex = 's/\x0D\x0A/\x0A/g'
`#{adb} shell screencap -p | perl -pe '#{regex}' > screen.png`
`bundle exec ruby png_reader.rb`
puts `scheme --silent < hoplite.scm`