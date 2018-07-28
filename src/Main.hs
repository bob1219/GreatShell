-- Copyright 2018 Daiki Yoshida
--
-- This file is part of GreatShell.
--
-- GreatShell is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- GreatShell is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with GreatShell. If not, see <http://www.gnu.org/licenses/>.

module Main (main) where

import GShell.Constants	(version, unknownException)
import GShell.Commands	(commandProcess, tokenizeCommand)
import System.IO.Error	(isEOFError, catchIOError)
import System.IO	(hSetBuffering, stdout)

main :: IO ()
main = do	hSetBuffering stdout NoBuffering

		putStrLn $ "GreatShell Version " ++ version
		putStrLn "Copyright 2018 Daiki Yoshida"
		putChar '\n'

		putStrLn "This program comes with ABSOLUTELY NO WARRANTY."
		putStrLn "This is free software, and you are welcome to redistribute it under certain conditions."
		putChar '\n'

		loop

loop :: IO ()
loop = do	putChar '>'
		((tokenizeCommand <$> getLine) >>= (\tokens ->	if null tokens
									then loop
									else commandProcess tokens))
			`catchIOError` (\e ->	if isEOFError e
							then loop
							else unknownException e)

		putChar '\n'
		loop
