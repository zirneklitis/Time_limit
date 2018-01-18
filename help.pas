(*
 * Version: 00.05.01.
 * Author: Kārlis Kalviškis, 2018.01.18. 05:09
 * License: GPLv3
 *)

unit help;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Ipfilebroker, IpHtml, Forms, Controls, Graphics
  , Dialogs, StdCtrls, DefaultTranslator, ComCtrls
  , fileinfo, resource, winpeimagereader, elfreader, machoreader, LCLVersion
;

type

  { TFHelp }

  TFHelp = class(TForm)
    IpHtmlDataProvider1: TIpHtmlDataProvider;
    HTMLHotKey: TIpHtmlPanel;
    HTMLSystem: TIpHtmlPanel;
    PHelp: TPageControl;
    PTHotKey: TTabSheet;
    PTAbout: TTabSheet;
    procedure FormCreate(Sender: TObject);
  private
    procedure ShowHotKey(HelpSrc: string; WhereToShow: TIpHtmlPanel);
    function CreateHTMLTable (FirstColumn: TStringArray; SecondColumn : TStringArray;
      ColTag1B: String; ColTag1E: String; ColTag2B: String; ColTag2E: String) : String;
    function SystemInfo () : String;
    function IsThereVersionInfo(Instance: THandle):boolean;

  public

  end;

var
  FHelp: TFHelp;
  FileVerInfo: TFileVersionInfo;

implementation

{$R *.lfm}

{ TFHelp }

(*
  To use 'Main' window's objects.
  Should be placed here not to run in circular refernce.
*)
uses basewindow;

resourcestring

  HKey00A = 'Be patient!';
  HKey01A = 'Any interactive changes are carried out within 1 second interval.';
  HKey02A = '[Space], [s], Mouse click';
  HKey02B = 'start/stop the timer.';
  HKey03A = '[F11], [f], Mouse double-click';
  HKey03B = 'full screen on/off.';
  HKey04A = '[Esc]';
  HKey04B = 'exit.';
  HKey05A = '[b]';
  HKey05B = 'border-less on/off.';
  HKey06A = '[F1], [h]';
  HKey06B = 'this help.';
  HKey07A = '[m], Mouse right click';
  HKey07B = 'settings window.';
  HKey08A = '[Left], [Up]';
  HKey08B = 'increase time by 1 minute.';
  HKey09A = '[Right], [Down]';
  HKey09B = 'decrease time by 1 minute.';
  HKey10B = 'You can change current time with the mouse wheel as well.';
  HKey11A = '[r], Mouse middle click';
  HKey11B = 'reset timer.';
  SInfo01 = 'Product name';
  SInfo02 = 'Original filename';
  SInfo03 = 'File version';
  SInfo04 = 'File description';
  SInfo05 = 'Legal copyright';
  SInfo06 = 'Comments';
  BInfo01 = 'Built for';
  BInfo02 = 'Free Pascal Compiler version';
  BInfo03 = 'Lazarus version';
  BInfo04 = 'Compiled on';
procedure TFHelp.FormCreate(Sender: TObject);
var
   html_head : String;
   html_end  : String;
begin
  html_head := '<HTML>'
        +'<HEAD>'
        +'<meta content="text/html; charset=utf-8" http-equiv="Content-Type">'
        +'</HEAD>'
        +'<BODY BGCOLOR=#FFFFAA>';
  html_end := '</BODY>'
        +'</HTML>';

  // List of Hot Keys
  HTMLHotKey.DefaultFontSize := Self.Font.Size;
  ShowHotKey(html_head
        + '<I><B>' + HKey00A + '</B><BR>' + HKey01A + '</I>'
        + CreateHTMLTable(
        TStringArray.Create(HKey02A, HKey03A, HKey04A, HKey05A, HKey06A, HKey07A, HKey08A, HKey09A, '', HKey11A)
        ,
        TStringArray.Create(HKey02B, HKey03B, HKey04B, HKey05B, HKey06B, HKey07B, HKey08B, HKey09B, HKey10B, HKey11B)
        , '<B><TT>', '</TT></B>', '', '')
        + html_end
        , HTMLHotKey);

  // System information
  HTMLSystem.DefaultFontSize := Self.Font.Size;
  ShowHotKey(html_head
        + '<I><B>' + FTimer.Caption + '</B></I><HR>'
        + SystemInfo
        + html_end
        , HTMLSystem);
end;

procedure TFHelp.ShowHotKey(HelpSrc: string; WhereToShow: TIpHtmlPanel);
// Creates a html page
var
  ss: TStringStream;
  NewHTML: TIpHtml;
  ThisPanel: TIpHtmlPanel;
begin
  ss := TStringStream.Create(HelpSrc);
  ThisPanel := WhereToShow;
  try
    NewHTML := TIpHtml.Create;
    ThisPanel.SetHtml(NewHTML);
    NewHTML.LoadFromStream(ss);
  finally
    ss.Free;
  end;
end;

function TFHelp.CreateHTMLTable (FirstColumn: TStringArray; SecondColumn : TStringArray;
  ColTag1B: String; ColTag1E: String; ColTag2B: String; ColTag2E: String) : String;
// Creates 2 column HTML table
var
  htmlcode, bgcolour : String;
  i : Integer;
begin
     htmlcode := '<TABLE WIDTH=100% BORDER=0>'
        ;
   for i := 0 to Length(FirstColumn) - 1 do
   begin
      if bgcolour = '' then
         bgcolour := ' BGCOLOR=#FFFFDD'
      else
         bgcolour := '';
      htmlcode :=   htmlcode
        + '<TR VALIGN=TOP' +bgcolour + '>'
        + '<TD WIDTH=30%>' + ColTag1B +  FirstColumn[i] + ColTag1E + '</TD>'
        + '<TD WIDTH=3%>–</TD>'
        + '<TD WIDTH=67%>' + ColTag2B +  SecondColumn[i] + ColTag2E + '</TD>'
        + '</TR>'
        ;
   end;
   htmlcode :=   htmlcode
       + '</TABLE>'
       ;
   result := htmlcode;
end;

function TFHelp.SystemInfo () : String;
Var
  ColName : TStringArray;
  ColValue : TStringArray;
  ColLen : Integer;
Begin
  FileVerInfo:=TFileVersionInfo.Create(nil);
  if IsThereVersionInfo(HINSTANCE) then
     Begin
       FileVerInfo.ReadFileInfo;
       ColName := ColName.Create(SInfo01, SInfo02, SInfo03, SInfo04, SInfo05, SInfo06);
        with FileVerInfo.VersionStrings do
             ColValue := ColValue.Create(
             Values['ProductName']
             , Values['OriginalFilename']
             , Values['FileVersion']
             , Values['FileDescription']
             , Values['LegalCopyright']
             , Values['Comments']
             );
     end;
  ColLen :=  Length(ColName);
  SetLength(ColName, ColLen + 4);
  SetLength(ColValue, ColLen + 4);
  ColName[ColLen] := BInfo01;
  ColValue[ColLen] := {$I %FPCTARGETCPU%} + ' - ' + {$I %FPCTARGETOS%};
  ColName[ColLen + 1] := BInfo02;
  ColValue[ColLen + 1] :=  {$I %FPCVERSION%};
  ColName[ColLen + 2] := BInfo03;
  ColValue[ColLen + 2] :=  lcl_version;
  ColName[ColLen + 3] := BInfo04;
  ColValue[ColLen + 3] :=   {$I %DATE%} + ' ' + {$I %TIME%};

  Result :=  CreateHTMLTable(ColName, ColValue, '<i>', '</i>', '', '');
end;

function TFHelp.IsThereVersionInfo(Instance: THandle):boolean;
Begin
  If FindResource(Instance, PChar(PtrInt(1)), PChar(RT_VERSION)) = 0 Then
    Result := false
  else
    Result := true;
end;

end.

