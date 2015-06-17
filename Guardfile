# A sample Guardfile
# More info at https://github.com/guard/guard#readme

## Uncomment and set this to only include directories you want to watch
# directories %w(app lib config test spec features)

## Uncomment to clear the screen before every task
# clearing :on

## Guard internally checks for changes in the Guardfile and exits.
## If you want Guard to automatically start up again, run guard in a
## shell loop, e.g.:
##
##  $ while bundle exec guard; do echo "Restarting Guard..."; done
##
## Note: if you are using the `directories` clause above and you are not
## watching the project directory ('.'), then you will want to move
## the Guardfile to a watched dir and symlink it back, e.g.
#
#  $ mkdir config
#  $ mv Guardfile config/
#  $ ln -s config/Guardfile .
#
# and, you'll have to watch "config/Guardfile" instead of "Guardfile"

require 'pry'
require 'guard/haskell'
require 'guard/haskell/repl'

class ::Guard::Haskellish < ::Guard::Haskell
  class ::Guard::Haskell::Repl
    # Show dots rather than all tests when not in failing mode
    def reload_and_run_matching(pattern = nil)
      if run_command_and_wait_for_result(":reload\n")
        if pattern.nil?
          run_command_and_wait_for_result(":main --color --format=progress\n")
        else
          run_command_and_wait_for_result(":main --color --match #{pattern} --format=progress\n")
        end
      end
    end
  end
end

guard :haskellish, all_on_start: true do
  watch(%r{test/.+Spec\.l?hs$})
  watch(%r{src/.+\.l?hs$})
  watch(%r{\.cabal$})
end
