unit Unit_Conf;

{$mode ObjFPC}{$H+}

//Copyright (c) 2023 by Xelitan.com
//License: GNU GPL
//See the file LICENSE.txt, included in this distribution, for details.

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,Registry, ShlObj;

const
  Formats: array[0..5] of String = ('.bmp', '.gif', '.jpg', '.jpeg', '.jpe', '.png');
  APP_NAME = 'XelitanGUIPotrace';

type

  { TConfigDlg }

  TConfigDlg = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private

  public

  end;

var
  ConfigDlg: TConfigDlg;

implementation

{$R *.lfm}

{ TConfigDlg }

procedure TConfigDlg.Button1Click(Sender: TObject);
var i: Integer;
begin

  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;

    for i:=0 to High(Formats) do begin
      if OpenKey('\SystemFileAssociations\' + Formats[i] + '\shell\' + APP_NAME, true) then
        WriteString('', 'Convert to SVG with Potrace');

      if OpenKey('\SystemFileAssociations\' + Formats[i] + '\shell\' + APP_NAME + '\command', true) then
        WriteString('', '"' + Application.ExeName + '" "%1"');
    end;

  finally
    Free;
  end;

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

procedure TConfigDlg.Button2Click(Sender: TObject);
var i: Integer;
begin

  with TRegistry.Create do
  try
    RootKey := HKEY_CLASSES_ROOT;

    for i:=0 to High(Formats) do begin
      DeleteKey('\SystemFileAssociations\' + Formats[i] + '\shell\' + APP_NAME + '\command');

      DeleteKey('\SystemFileAssociations\' + Formats[i] + '\shell\' + APP_NAME);
    end;

  finally
    Free;
  end;

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

end.

