(*
 * Version: 00.09.13.
 * Author: Kārlis Kalviškis, 2024.06.27
 * License: GPLv3
 * Слава Україні!
 *)
program TimeLimit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  sysutils,
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  DefaultTranslator, //to enable translation
  Interfaces, // this includes the LCL widgetset
  fileinfo, Forms, runtimetypeinfocontrols,
  basewindow, settings, help, console;
{$R *.res}

(*
Strings for translation. Will be automaticaly inserted into *.po files.
The *.po files should be in "lang" directory. Use application like
"Virtaal" to generate *.mo files. The *.mo files should be in "language"
directory. The main template file is "lang/Time_limit.po". Language is
determined by the suffix of a file name,e.g., "Time_limit.lv.po" is for Latvian
translation. Translate and export the file as "Time_limit.lv.mo" respectively.
*)

resourcestring
  RstUsage = 'Usage';
  RstOption = 'OPTIONS';
  RstOptions = 'Options';
  RstReadINI = 'Read the specified configuration file.';
  RstStart = 'Starts the countdown.';
  RstRun = 'At the end of countdown launches another program.';
  RstExit = 'Exit at the end of countdown.';
  RstLang = 'Set the interface language (nn is the language code).';
  RstPause = 'Hit [Enter] to continue ...';
  RstClock = 'Start as an ordinary digital clock.';

var
  ConfigurationFile: String;
  StartCounter: Boolean;
  AllParameters: Integer;
  CmdToRun: String;
  ExitCounter: Boolean;
  ClockMode: Integer;
begin
  ExitCounter := false;
  StartCounter := false;
  ClockMode := -1;
  Application.Scaled:=True;
  If ParamCount > 0 then begin
       AllParameters := ParamCount;
       if
  Application.HasOption('config') then begin
         ConfigurationFile := Application.GetOptionValue('config');
         Dec(AllParameters);
         end;
       if Application.HasOption('s', 'start') then begin
         StartCounter := true;
         Dec(AllParameters);
         end;
       if Application.HasOption('run') then begin
         CmdToRun := Application.GetOptionValue('run');
         Dec(AllParameters);
         end;
       if Application.HasOption('e', 'exit') then begin
         ExitCounter := true;
         Dec(AllParameters);
         end;
       if Application.HasOption('l', 'lang') then begin
         // Maintained by Lazarus.
         // The language code is summed up as a separate parameter.
         AllParameters := AllParameters - 2;
        end;
       if Application.HasOption('clock') then begin
         ClockMode := 2;
         Dec(AllParameters);
        end;
       if (AllParameters > 0) or Application.HasOption('h', 'help') then begin
          // In Windows a new CMD window is opened to display commandline options
          {$IFDEF WINDOWS}
            AllocConsole;      // in Windows unit
            IsConsole := True; // in System unit
            SysInitStdIO;      // in System unit
          {$ENDIF}
          FileVerInfo:=TFileVersionInfo.Create(nil);
          FileVerInfo.ReadFileInfo;
          with FileVerInfo.VersionStrings do
               WriteLn(
               Values['ProductName'], ' - '
               , Values['FileVersion'], sLineBreak
               , Values['FileDescription'], ' '
               , Values['LegalCopyright'], sLineBreak
               , Values['Comments']
               );
           WriteLn('');
           WriteLn(RstUsage, ': ', ExtractFileName(ParamStr(0)), ' [', RstOption, '] [..]');
           WriteLn('');
           WriteLn(RstOptions, ':');
           WriteLn('');
           WriteLn('--clock             ', RstClock);
           WriteLn('');
           WriteLn('--config=file.ini   ', RstReadINI);
           WriteLn('');
           WriteLn('-e or --exit        ', RstExit);
           WriteLn('');
           WriteLn('--lang nn or -l nn  ', RstLang);
           WriteLn('');
           WriteLn('--run=command       ', RstRun);
           WriteLn('');
           WriteLn('-s or --start       ', RstStart);
           WriteLn('');
           WriteLn('');
           FreeAndNil(FileVerInfo);
          {$IFDEF WINDOWS}
             Write(RstPause);
             ReadLn;
          {$ENDIF}
           Halt;
       end;
  end;
  Application.Title:='Time limit (Countdown timer)';
    RequireDerivedFormResource := True;
    Application.Initialize;
    Application.CreateForm(TFTimer, FTimer);
    Application.CreateForm(TFConfig, FConfig);
    if ConfigurationFile <> '' then FConfig.LoadConfiguration(ConfigurationFile);
    if CmdToRun <> '' then begin
       FConfig.ChLaunch.Checked := true;
       FConfig.ECMDtoRun.Text := CmdToRun;
    end;
    FConfig.ChExit.Checked := ExitCounter;
    FTimer.RUNING := StartCounter;
    FConfig.RGrTimerMode.ItemIndex := ClockMode;
  Application.CreateForm(TFConsole, FConsole);
    Application.Run;
end.

