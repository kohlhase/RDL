%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% rdl_management.pl: contains the predicates to manage rdl 
%%%                    executable exportation 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                    Luca Compagna                    
%%%         (E-mail: compa@mrg.dist.unige.it)            
%%%                University of Genova                   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded(library(system)).
:- ensure_loaded(library(charsio)).
:- ensure_loaded(rdl).

% Files that we want to export 
export_file('problems.pl').
export_file('LICENSE').
export_file('README').
export_file('INSTALL').
export_file('tutorial.html').
export_file('rdl_setenv').
% export_file('ROADMAP').

% RDL Files
rdl_file('rdl.pl').
rdl_file('reduce.pl').
rdl_file('simplify.pl').
rdl_file('cs_extend.pl').
rdl_file('ccr.pl').

rdl_file('ordering.pl').
rdl_file('problem.pl').
rdl_file('base.pl').

rdl_file('rewrite.pl').
rdl_file('shostak.pl').
rdl_file('augment.pl').
rdl_file('fourier.pl').
rdl_file('normalise_lineq.pl').

rdl_file('non_linear.pl').
rdl_file('eq.pl').
rdl_file('la.pl').
rdl_file('eq_la.pl').

library_file('system.pl').
library_file('lists.pl').
library_file('clpfd.pl').
library_file('ordsets.pl').
library_file('terms.pl').
library_file('assoc.pl').

% It may be useless
% library_sprt_filename('libsprt38.so').
sicstus_directory('/usr/local/sicstus').
sicstus_version_rdirectory('sicstus-3.8.5').

rdl_interface_filename_binary('rdl_interface.ql').
rdl_interface_filename('rdl_interface.pl').

rdl_exec_filename('rdl').

rdl_savestate_filename('rdl').

compile_rdl :-
	setof(F,rdl_file(F),Fs),
	compile(Fs).

make_rdl_save_state :-
	compile_rdl,
	rdl_savestate_filename(RDL),
        rdl_banner(X),
	version(X),
	save_program(RDL),
	halt.

% Generates the executable file
% make_rdl_executable :-
% 	working_directory(Path, Path),
% 	make_rdl_executable(Path).

make_rdl_executable(RD,relative) :-
	working_directory(Path, Path),
	atom_concat(Path,'/',Tmp),
	atom_concat(Tmp,RD,AbsD),
	make_rdl_executable(AbsD).

make_rdl_executable(AbsD,absolute) :-
	make_rdl_executable(AbsD).

make_rdl_executable(AbsP) :-

	% Get the current working directory 
	working_directory(Path, Path),

	atom_concat(Path,'/lib',DLIB),

	% Make lib directory
	(\+file_exists(DLIB) -> 
	    make_directory_recursive(DLIB)
	;
	    true
	),

	sicstus_directory(DSP),
	atom_concat(DSP,'/lib',DSPLIB),

%% It may be useless
% 	% Copies the file LIBSPRT into the directory DLIB
% 	library_sprt_filename(LIBSPRT),
% 	copy_files_from_to([LIBSPRT],DSPLIB,DLIB),

	sicstus_version_rdirectory(RSPVERS),
	atom_concat(DLIB,'/',TDVERS),
	atom_concat(TDVERS,RSPVERS,DVERS),

	atom_concat(DSPLIB,'/',TDSPVERS),
	atom_concat(TDSPVERS,RSPVERS,DSPVERS),

	% Make sicstus-x.y.z directory
	(\+file_exists(DVERS) -> 
	    make_directory(DVERS)
	;
	    true
	),

	atom_concat(DVERS,'/bin',DBIN),

	% Make bin directory
	(\+file_exists(DBIN) -> 
	    make_directory(DBIN)
	;
	    true
	),

	% Copies the file 'sprt.sav' into the directory DBIN 
	format_to_chars("cp ~p/bin/sprt.sav ~p/.",[DSPVERS,DBIN],CCMD_DIR),
	atom_chars(CMD_DIR,CCMD_DIR),
	exec(CMD_DIR,[pipe(_),pipe(_),pipe(_)],_),	
	
	atom_concat(DVERS,'/library',DLIBRARY),
	% Make 'library' directory
	(\+file_exists(DLIBRARY) -> 
	    make_directory(DLIBRARY)
	;
	    true
	),
	atom_concat(DSPVERS,'/library',DSPLIBRARY),
	% Copies library file into 'library' directory
	setof(LF,library_file(LF),LFs),
	copy_files_from_to(LFs,DSPLIBRARY,DLIBRARY),

	rdl_exec_filename(RDL),
	rdl_interface_filename(RDL_INTERFACE),

% 	% Copies rdl files in the destination directory 
 	setof(F,rdl_file(F),Fs),
	% Compile rdl files and generates the opportunaly file *.ql
	op(701,xfx,<--),
	fcompile([RDL_INTERFACE|Fs]),

	% Build the line command for creating executable file
	build_sequence_compile_files([RDL_INTERFACE|Fs],STR),
	atom_concat(DSP,'/bin/spld --main=load ',TTCMD_SPLD),
	atom_concat(TTCMD_SPLD,STR,TTTCMD_SPLD),
	atom_concat(TTTCMD_SPLD,' -v --moveable --static -o ',TTTTCMD_SPLD),
	atom_concat(TTTTCMD_SPLD,RDL,CMD_SPLD),

	exec(CMD_SPLD,[pipe(_),pipe(_),pipe(_)],PSPLD),
	wait(PSPLD,_),

	% Make destination directory
	(\+file_exists(AbsP) -> 
	    make_directory_recursive(AbsP)
	;
	    true
	),
 	% Copies export files in the destination directory 
 	setof(EF,export_file(EF),EFs),
 	copy_files(EFs,AbsP),

	% Remove the old 'lib' directory from the destination 
	% directory
	atom_concat(AbsP,'/lib',AbsLIB),
	(file_exists(AbsLIB) -> 
	    atom_concat('rm -rf ',AbsLIB,CMD_RM_LIB),
	    exec(CMD_RM_LIB,[pipe(_),pipe(_),pipe(_)],PRMLIB),	
	    wait(PRMLIB,_)
	;
	    true
	),
	
	format_to_chars("mv lib ~p/lib",[AbsP],CCMD_MV_DIR),
	atom_chars(CMD_MV_DIR,CCMD_MV_DIR),
	exec(CMD_MV_DIR,[pipe(_),pipe(_),pipe(_)],PMVDIR),	
	wait(PMVDIR,_),

	atom_concat('mv',STR,TCMD_MV_BIN),
	atom_concat(TCMD_MV_BIN,AbsP,TTCMD_MV_BIN),
	atom_concat(TTCMD_MV_BIN,'/.',CMD_MV_BIN),
	exec(CMD_MV_BIN,[pipe(_),pipe(_),pipe(_)],PMVBIN),	
	wait(PMVBIN,_),

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% These lines can be commented when you comment
%%% the line ensure_loaded(rdl) in the file 
%%% rdl_interface.pl
% 	copy_files(['rdl.pl'],AbsP),

 	move_files([RDL],AbsP),

	% Check if the environment variables are set right
	atom_concat(AbsP,'/lib/',TEXP_SP_PATH),
	atom_concat(TEXP_SP_PATH,RSPVERS,EXP_SP_PATH),
	(\+environ('SP_PATH', EXP_SP_PATH) -> 
	    nl,
	    nl,
	    write('To run the system RDL you have to set the environment variable:'),
	    nl,
	    write('export SP_PATH='),
	    write(EXP_SP_PATH),
	    nl,
	    nl,
 	    nl
	;
	    true
	),
	halt.
	
build_sequence_compile_files(Fs,STR):-
	build_sequence_compile_files(Fs,STR,' ').

build_sequence_compile_files([],STR,STR).
build_sequence_compile_files([F|Fs],STR,PreSTR):-
	atom_concat(TF,'.pl',F),
	atom_concat(TF,'.ql ',CF),
	atom_concat(PreSTR,CF,CurrSTR),
	build_sequence_compile_files(Fs,STR,CurrSTR).

% Generates the relative directory RP and the tgz 
% file F which contains the executable file of the rdl, 
% the binary file of its and the export_file(..)
generate_tgz(F,RP) :-
	export_rdl(RP),
	format_to_chars("tar -cvzf ~p ~p", [F,RP], CC),
	atom_chars(CTar,CC),
	exec(CTar,[pipe(_),pipe(_),pipe(_)],PTar),
	wait(PTar,_),
	format("-----------------------------------------------\n",[]),
	format("Created the compressed file ~p\n",[F]),
	format("-----------------------------------------------\n\n",[]),
	halt.

% Generates the relative directory RP and the tgz 
% file F which contains the binary file of the rdl 
% and the export_file(..)
generate_tgz(F,RP) :-
	export_rdl_savestate(RP),
	format_to_chars("tar -cvzf ~p ~p", [F,RP], CC),
	atom_chars(CTar,CC),
	exec(CTar,[pipe(_),pipe(_),pipe(_)],PTar),
	wait(PTar,_),
	format("-----------------------------------------------\n",[]),
	format("Created the compressed file ~p\n",[F]),
	format("-----------------------------------------------\n\n",[]),
	halt.

% Export the export_file(F) and the executable file of rdl into 
% the directory RP. Useful for exporting on the web.
export_rdl(RP):-
	setof(F,export_file(F),Fs),
	export_rdl(RP,Fs),
	format("-----------------------------------------------\n",[]),
	format("Created the relative directory ~p\n",[RP]),
	format("-----------------------------------------------\n\n",[]).

export_rdl(RP,OtherFiles):-

	% Get the current working directory 
	working_directory(Path, Path),

	% Computes the absolute path AbsP of 
	% the destination directory  
	atom_chars(RP,CRP),
	atom_chars(Path,CPath),
	atom_chars('/',[CS]),
	append(CPath,[CS|CRP],CAbsP),
	atom_chars(AbsP,CAbsP),
		
	% Make destination directory
	(\+file_exists(AbsP) -> 
	    make_directory(AbsP)
	;
	    true
	),

	% Binary files generates in the directory
	% AbsP
	setof(F,rdl_file(F),Fs),
	rdl_interface_filename(RDL_INTERFACE),
	copy_files([RDL_INTERFACE|Fs],AbsP),
	working_directory(Path, AbsP),
	op(701,xfx,<--),
	fcompile([RDL_INTERFACE|Fs]),
	delete_files(Fs,AbsP),
	% Create the executable
	make_rdl_executable,
	working_directory(AbsP, Path),
	
	% Copy export files and executable file in the destination 
	% directory AbsP
	copy_files(OtherFiles,AbsP).

% Export the export_file(F) and the binary files of rdl into 
% the directory RP. Useful for exporting on the web.
export_rdl_savestate(RP):-
	setof(F,export_file(F),Fs),
	export_rdl_savestate(RP,Fs),
	format("-----------------------------------------------\n",[]),
	format("Created the relative directory ~p\n",[RP]),
	format("-----------------------------------------------\n\n",[]).

export_rdl_savestate(RP,OtherFiles):-
	% Build the save state of rdl
	compile_rdl,
	rdl_savestate_filename(RDL),
        rdl_banner(X),
	version(X),
	save_program(RDL),
	
	% Get the current working directory 
	working_directory(Path, Path),

	% Computes the absolute path AbsP of 
	% the destination directory  
	atom_chars(RP,CRP),
	atom_chars(Path,CPath),
	atom_chars('/',[CS]),
	append(CPath,[CS|CRP],CAbsP),
	atom_chars(AbsP,CAbsP),
		
	% Make destination directory
	(\+file_exists(AbsP) -> 
	    make_directory(AbsP)
	;
	    true
	),
	
	% Binary files generates in the directory
	% AbsP
	move_files([RDL],AbsP),
	setof(F,rdl_file(F),Fs),
	copy_files(Fs,AbsP),
	working_directory(Path, AbsP),
	op(701,xfx,<--),
	fcompile(Fs),
	delete_files(['rdl.ql'|Fs],AbsP),
	working_directory(AbsP, Path),

	% Copy export files in the destination 
	% directory AbsP
	copy_files(OtherFiles,AbsP).

make_directory_recursive(DIRs):-
	atom(DIRs),
	DIRs \= _/_,
	atom_chars('/',[SLASH]),
	write_to_chars(DIRs,CDIRs),
	append(CPre,[SLASH|CDIR],CDIRs),
	\+member(SLASH,CDIR),
	(CPre==[] -> 
	    (\+file_exists(DIRs) -> 
		make_directory(DIRs)
	    ;
	        true
	    );
	    atom_chars(Pre,CPre),
	    make_directory_recursive(Pre),
	    (\+file_exists(DIRs) -> 
		format_to_chars("mkdir ~p",[DIRs],CCMD),
		atom_chars(CMD,CCMD),
		exec(CMD,[pipe(_),pipe(_),pipe(_)],PMD),
		wait(PMD,_);
		true
	    )
	).

make_directory_recursive(DIRs/DIR):-
	make_directory_recursive(DIRs),
	(\+file_exists(DIR) -> 
	    format_to_chars("mkdir ~p/~p",[DIRs,DIR],CCMD),
	    atom_chars(CMD,CCMD),
	    exec(CMD,[pipe(_),pipe(_),pipe(_)],_)
	;
	    true
	).

make_directory_recursive(DIR):-
	DIR \= _/_,
	(\+file_exists(DIR) -> 
	    make_directory(DIR)
	;
	    true
	).
	

delete_files([],_).
delete_files([F|Fs],Directory):-
	format_to_chars("~p/~p", [Directory,F], Code),
	atom_chars(NewF,Code),
	(file_exists(NewF) -> 
	    delete_file(NewF)
	;
	    nl,
	    format("The file ~p doesn't exist. So it can't be delete",[NewF]),
	    nl
	),
	delete_files(Fs,Directory).

move_files([],_).
move_files([F|Fs],DestDir):-
	format_to_chars("mv ~p ~p/~p", [F,DestDir,F], CommandCode),
	atom_chars(Command,CommandCode),
	working_directory(Dir,Dir),
 	format_to_chars("~p/~p", [Dir,F], FCode),
	atom_chars(DirF,FCode),
	(file_exists(DirF) -> 
	    exec(Command,[pipe(_),pipe(_),pipe(_)],_)
	;
	    nl,
	    format("The file ~p doesn't exist. So it can't be moved",[DirF]),
	    nl
	),
	move_files(Fs,DestDir).

copy_files_from_to(Fs,From,To):-
	working_directory(Old,From),
	copy_files(Fs,To),
	working_directory(_,Old).

copy_files([],_).
copy_files([F|Fs],DestDir):-
	format_to_chars("cp ~p ~p/~p", [F,DestDir,F], CommandCode),
	atom_chars(Command,CommandCode),
	working_directory(Dir,Dir),
 	format_to_chars("~p/~p", [Dir,F], FCode),
	atom_chars(DirF,FCode),
	(file_exists(DirF) -> 
	    exec(Command,[pipe(_),pipe(_),pipe(_)],_)
	;
	    nl,
	    format("The file ~p doesn't exist. So it can't be copied",[DirF]),
	    nl
	),
	copy_files(Fs,DestDir).




