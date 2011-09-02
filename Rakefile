task :default => [:test]

task :test do
  Dir.chdir("ori")
  Dir["*.[he]rl"].each do |f|
    puts f
	  `sed "s/nick/usr/g" #{f} | sed "s/Nick/Usr/g" > #{"../src/" + f}`
  end
end
