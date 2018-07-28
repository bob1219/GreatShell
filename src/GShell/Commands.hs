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

module GShell.Commands (commandProcess, tokenizeCommand) where

import System.IO	(hPutStrLn, stderr)
import System.IO.Error	(catchIOError)
import GShell.Constants	(unknownException)
import System.Directory	(removeFile, copyFileWithMetadata, renameFile, createDirectory, doesFileExist, removeDirectory)

commandProcess :: [String] -> IO ()
commandProcess []		=	error "got empty list"
commandProcess (token:tokens)	=	case token of	"mkfile"	-> run tokens 1 (command_mkfile $ tokens !! 0)
							"rmfile"	-> run tokens 1 (command_rmfile $ tokens !! 0)
							"cpfile"	-> run tokens 2 (command_cpfile (tokens !! 0) (tokens !! 1))
							"renfile"	-> run tokens 2 (command_renfile (tokens !! 0) (tokens !! 1))
							"mkdir"		-> run tokens 1 (command_mkdir $ tokens !! 0)
							"rmdir"		-> run tokens 1 (command_rmdir $ tokens !! 0)
							"cpdir"		-> run tokens 2 (command_cpdir (tokens !! 0) (tokens !! 1))
							"rendir"	-> run tokens 2 (command_rendir (tokens !! 0) (tokens !! 1))
							"view"		-> run tokens 1 (command_view $ tokens !! 0)
							"chcwd"		-> run tokens 1 (command_chcwd $ tokens !! 0)
							"pcwd"		-> command_pcwd
							"list"		-> run tokens 2 (command_list (tokens !! 0) (tokens !! 1))
							"version"	-> command_version
							"path"		-> run tokens 2 $ command_path tokens
							"exit"		-> command_exit
							_		-> run tokens 2 $ exec tokens

run :: [String] -> Int -> IO () -> IO ()
run tokens n f = if (length tokens) < n then commandLineError "few args" else f

commandLineError :: String -> IO ()
commandLineError message = hPutStrLn stderr $ "Error: " ++ message

command_mkfile :: FilePath -> IO ()
command_mkfile filename = (doesFileExist filename) >>= (\exists ->	if exists
										then	commandLineError "that file already exists"
										else	((writeFile filename "")
												`catchIOError` (\e -> case e of _	| isFullError e		-> commandLineError "your device is full"
																	| isIllegalOperation e	-> commandLineError "that operation is illegal"
																	| isPermissionError e	-> commandLineError "you do not have the permission"
																	| isAlreadyInUseError e	-> commandLineError "that file already in use"
																	| otherwise		-> unknownException e)))

command_rmfile :: FilePath -> IO ()
command_rmfile filename =	(removeFile filename)
					`catchIOError` (\e -> case e of _	| isDoesNotExistError e	-> commandLineError "that file does not exist"
										| isAlreadyInUseError e	-> commandLineError "that file already in use"
										| isIllegalOperation e	-> commandLineError "that operation is illegal"
										| isPermissionError e	-> commandLineError "you do not have the permission"
										| otherwise		-> unknownException e)

command_cpfile :: FilePath -> FilePath -> IO ()
command_cpfile src dst =	(copyFileWithMetadata src dst)
					`catchIOError` (\e -> case e of _	| isDoesNotExistError e	-> commandLineError "that source-file does not exist"
										| isAlreadyInUseError e	-> commandLineError "that files already in use"
										| isFullError e		-> commandLineError "your device is full"
										| isIllegalOperation e	-> commandLineError "that operation is illegal"
										| isPermissionError e	-> commandLineError "you do not have the permission"
										| otherwise		-> unknownException e)

command_renfile :: FilePath -> FilePath -> IO ()
command_renfile src dst =	(renameFile src dst)
					`catchIOError` (\e -> case e of _	| isDoesNotExistError e	-> commandLineError "that source-file does not exist"
										| isAlreadyInUseError e	-> commandLineError "that files already in use"
										| isFullError e		-> commandLineError "your device is full"
										| isIllegalOperation e	-> commandLineError "that operation is illegal"
										| isPermissionError e	-> commandLineError "you do not have the permission"
										| otherwise		-> unknownException e)

command_mkdir :: FilePath -> IO ()
command_mkdir dirname =	(createDirectory dirname)
				`catchIOError` (\e -> case e of _	| isAlreadyExistsError e	-> commandLineError "that directory already exists"
									| isFullError e			-> commandLineError "your device is full"
									| isIllegalOperation e		-> commandLineError "that operation is illegal"
									| isPermissionError e		-> commandLineError "you do not have the permission"
									| otherwise			-> unknownException e)

command_rmdir :: FilePath -> IO ()
command_rmdir dirname =	(removeDirectory dirname)
				`catchIOError` (\e -> case e of _	| isDoesNotExistError e	-> commandLineError "that directory does not exist"
									| isAlreadyInUseError e	-> commandLineError "that directory already in use"
									| isIllegalOperation e	-> commandLineError "that operation is illegal"
									| isPermissionError e	-> commandLineError "you do not have the permission"
									| otherwise		-> unknownException e)
