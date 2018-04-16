@echo off

if %1 == -one goto one
if %1 == -all goto all 
goto usage 

:all
erl -pa ebin -s create_table create_all -s init stop
goto exit

:one
erl -pa ebin -s create_table create_one -s init stop
goto exit

:usage
echo Usage: create_table.bat Option
echo Option: -all	create all tables
echo 		 arg_table_name	create the specified table 

:exit