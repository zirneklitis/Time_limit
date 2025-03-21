(*
 * Version: 00.10.00.
 * Author: Kārlis Kalviškis, 2025.03.16
 * License: GPLv3
 *)

(*
 * Any setting to remember are registered in each form's property
 * "SessionProperties".
 * Misc/TIniPropStorage should be added to the form.
 * This object is renamed as "RememberSetings".
 *)

unit console;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Spin, IniPropStorage;

type

  { TFConsole }

  TFConsole = class(TForm)
    RememberSetings: TIniPropStorage;
    BConfig: TButton;
    BRestart: TButton;
    BShowClock: TToggleBox;
    BStart: TToggleBox;
    EMinutes: TFloatSpinEdit;
    LPClock: TLabel;
    PPClock: TPanel;
    RGrTimerMode: TRadioGroup;
    procedure BConfigClick(Sender: TObject);
    procedure BRestartClick(Sender: TObject);
    procedure BShowClockClick(Sender: TObject);
    procedure BStartClick(Sender: TObject);
    procedure EMinutesExit(Sender: TObject);
    procedure EMinutesKeyPress(Sender: TObject; var Key: char);
    procedure FormCreate(Sender: TObject);
     procedure FormResize(Sender: TObject);
    procedure RGrTimerModeClick(Sender: TObject);
  private

  public

  end;

var
  FConsole: TFConsole;

implementation

{$R *.lfm}

{ TFConsole }

(*
  To use 'Main' and window's objects.
  Should be placed here not to run in circular refernce.
*)
uses settings, basewindow;

resourcestring

  RstRemainingTime = 'Remaining time (MM:SS)';
  RstElapsedTime = 'Time elapsed (MM:SS)';
  RstClockTime = 'Clock mode (HH:MM)';


procedure TFConsole.BConfigClick(Sender: TObject);
begin
  FConsole.Hide;
  FConfig.Show;
end;

procedure TFConsole.BRestartClick(Sender: TObject);
begin
    FConfig.BRestart.Click;
end;


procedure TFConsole.BShowClockClick(Sender: TObject);
begin
    FConfig.BShowClock.Checked := Not FConfig.BShowClock.Checked;
    BShowClock.Caption := FConfig.BShowClock.Caption
end;

procedure TFConsole.BStartClick(Sender: TObject);
begin
    FConfig.BStart.Checked := Not FConfig.BStart.Checked;
end;

procedure TFConsole.EMinutesExit(Sender: TObject);
begin
    FConfig.EMinutes.Value:=EMinutes.Value;
    FConfig.BSettingsA.Click;
end;

procedure TFConsole.EMinutesKeyPress(Sender: TObject; var Key: char);
begin
       case Key of
       //[Enter]
       #13:
            begin
            FConfig.EMinutes.Value:=EMinutes.Value;
            FConfig.BSettingsARR.Click;
            end;
     end;

end;

procedure TFConsole.FormCreate(Sender: TObject);
begin
    RGrTimerMode.Items.Add(RstRemainingTime);
    RGrTimerMode.Items.Add(RstElapsedTime);
    RGrTimerMode.Items.Add(RstClockTime);
 end;


procedure TFConsole.FormResize(Sender: TObject);
    var
    i: Integer;
begin
    With FConsole do
    Begin
      For i:=0 to ComponentCount-1 do
        If Components[i] is TToggleBox then
          begin
            (Components[i] as TToggleBox).Height := Height div 9;
          end
        Else If Components[i] is TButton then
          begin
            (Components[i] as TButton).Height := Height div 9;
          end
        Else If Components[i] is TFloatSpinEdit then
          begin
            (Components[i] as TFloatSpinEdit).Height := Height div 9;
          end;
        BStart.Width := Width div 3;
        EMinutes.Width := Width div 3;
        Font.Height := Round(BShowClock.Height / 2.1);
        PPClock.Height := Height - 3 * BShowClock.Height - RGrTimerMode.Height;
    end;

end;

procedure TFConsole.RGrTimerModeClick(Sender: TObject);
begin
  FConfig.RGrTimerMode.ItemIndex := RGrTimerMode.ItemIndex;
end;


end.

