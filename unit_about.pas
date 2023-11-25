unit Unit_About;

{$mode ObjFPC}{$H+}

//Copyright (c) 2023 by Xelitan.com
//License: GNU GPL
//See the file LICENSE.txt, included in this distribution, for details.

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls;

type

  { TAboutDlg }

  TAboutDlg = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
  private

  public

  end;

var
  AboutDlg: TAboutDlg;

implementation

{$R *.lfm}

end.

