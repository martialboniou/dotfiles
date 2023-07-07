IRB_START_TIME = Time.now
ARGV.concat [ "--readline" ]

#require 'pp'
#require 'duration'
require 'rubygems' rescue nil
require 'wirble'
#require 'irb/completions'
#require 'irb/ext/save-history'

Wirble.init(:skip_prompt => true, :skip_history => true)
Wirble.colorize

IRB.conf[:AUTO_INDENT] = true
#require 'utility_belt'
#edit(:mvim)
#UtilityBelt::Themes.background(:dark)
#

#at_exit { puts Duration.new(Time.now - IRB_START_TIME) }
at_exit { puts Time.now - IRB_START_TIME }
