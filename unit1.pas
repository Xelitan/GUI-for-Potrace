unit Unit1;

{$mode objfpc}{$H+}

//Copyright (c) 2023 by Xelitan.com
//License: GNU GPL
//See the file LICENSE.txt, included in this distribution, for details.

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  Buttons, StdCtrls, BGRABitmap, BGRASVG, ImageBox, process, BGRABitmapTypes,
  zstream;

type
  { TForm1 }
  TForm1 = class(TForm)
    Edit3: TEdit;
    Edit5: TEdit;
    Label3: TLabel;
    Label5: TLabel;
    Panel3: TPanel;
    Edit2: TEdit;
    Label2: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    SaveDialog1: TSaveDialog;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    Splitter1: TSplitter;
    StatusBar1: TStatusBar;
    UpDown2: TUpDown;
    UpDown3: TUpDown;
    UpDown5: TUpDown;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
  private

  public
    TRACE_EXE: String;
    Zoom: Integer;
    Before, After: TBox;
    CurrentFile: String;
    OrgBmp: TBitmap;
    SvgMem: RawByteString;
    SvgSize: Integer;
    JustCreated: Boolean;

    procedure OpenFile(Filename: String);

    procedure ResizeForm;
    function LoadImg(Filename: String): TBitmap;
    function LoadSvg(Filename: String): TBitmap;

    function GetTempName(Ext: String): String;
    procedure BgraToBitmap(Bgra: TBgraBitmap; Bmp: TBitmap);
  end;

var
  Form1: TForm1;

implementation

uses unit_conf, unit_about;

{$R *.lfm}

{ TForm1 }

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
  if not OpenDialog1.Execute then Exit;

  OpenFile(OpenDialog1.Filename);
end;

procedure TForm1.OpenFile(Filename: String);
var TempBmp: TBgraBitmap;
begin
  try
    TempBmp := TBgraBitmap.Create(Filename);
  except
    Exit;
  end;

  CurrentFile := Filename;

  BgraToBitmap(TempBmp, OrgBmp);
  TempBmp.Free;

  Before := TBox.Create(Form1);
  Before.SetBitmap(OrgBmp);

  Before.Parent := Panel4;
  Before.Align := alClient;
  Before.ZoomFit;

  After := TBox.Create(Form1);

  After.Parent := Panel5;
  After.Align := alClient;

  Paint;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  TRACE_EXE := ExtractFilePath(Application.ExeName) + 'XelitanPotrace.exe';

  if not FileExists(TRACE_EXE) then begin
    ShowMessage('XelitanPotrace.exe not found');
    Application.Terminate;
  end;

  Zoom := 100;

  OrgBmp := TBitmap.Create;
  OrgBmp.PixelFormat := pf24bit;

  JustCreated := True;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  OrgBmp.Free;
end;

procedure TForm1.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  OpenFile(FileNames[0]);
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  ResizeForm;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  if not JustCreated then Exit;

  if ParamCount > 0 then OpenFile(ParamStr(1));

  JustCreated := False;
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
var F: TFileStream;
    FName: String;
begin
  FName := ChangeFileExt(CurrentFile, '.svg');

  try
    F := TFileStream.Create(FName, fmCreate or fmShareDenyWrite);
    F.Write(SvgMem[1], SvgSize);
    F.Free;
  except
    Showmessage('Could not save');
  end;
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
  AboutDlg.Show;
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
var FStream: TFileStream;
    FName: String;
    Ext: String;
    Gz: TGZFileStream;
begin
  if not SaveDialog1.Execute then Exit;

  FName := SaveDialog1.Filename;

  Ext := LowerCase(ExtractFileExt(FName));

  if Ext = '.svg' then begin
    FStream := TFileStream.Create(FName, fmCreate or fmShareDenyWrite);
    FStream.Write(SvgMem[1], SvgSize);
    FStream.Free;
  end
  else begin //svgz
    Gz := TGZFileStream.create(FName, gzopenwrite);
    Gz.Write(SvgMem[1], SvgSize);
    Gz.Free;
  end;
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);
begin
  ConfigDlg.Show;
end;

procedure TForm1.SpeedButton7Click(Sender: TObject);
var OutStr: String;
    TmpBmp1, TmpBmp2: TBgraBitmap;
    TempBmp: String;
    TempSvg: String;
    Bmp: TBitmap;
    Smooth: Integer;
    Noise: Integer;
    Simplify: Integer;
    WW, HH: Extended;
begin
  Smooth := UpDown2.Position;
  Noise := UpDown3.Position;         //-t

  Simplify := UpDown5.Position;      //-a

  TempBmp := GetTempName('bmp');
  TempSvg := GetTempName('svg');

  WW := (OrgBmp.Width-100) / 100;
  HH := (OrgBmp.Height-100) / 100;

  Smooth := 100 - Smooth;

  TmpBmp1 := TBgraBitmap.Create(OrgBmp);
  TmpBmp2 := TmpBmp1.Resample(Round(100 + WW*Smooth), Round(100 + HH*Smooth));
  TmpBmp1.FRee;

  TmpBmp2.SaveToFile(TempBmp);
  TmpBmp2.Free;



  RunCommand(TRACE_EXE, ['-W 20 -t '+ IntToStr(Noise) + ' -a '+ IntToStr(Simplify) + ' -s -o "' + TempSvg + '" "' + TempBmp + '" '], OutStr, [poWaitOnExit], swoHIDE);


  Bmp := LoadSvg(TempSvg);
  DeleteFile(TempSvg);
  DeleteFile(TempBmp);


  After.SetBitmap(Bmp);
  After.ZoomFit;
  Bmp.Free;
end;

procedure TForm1.ResizeForm;
begin
  Panel4.Height := Panel3.Height div 2;
end;

function TForm1.LoadImg(Filename: String): TBitmap;
var Bgra: TBgraBitmap;
begin
  Bgra := TBgraBitmap.Create(Filename);

  Result := TBitmap.Create;


  Result.SetSize(Bgra.Width, Bgra.Height);
  Result.Canvas.Brush.Color := clWhite;
  Result.Canvas.FillRect(0,0, Result.Width, Result.Height);

  Bgra.Draw(Result.Canvas, 0,0, false);

  Bgra.Free;
end;

function TForm1.LoadSvg(Filename: String): TBitmap;
var Bgra: TBgraBitmap;
    F: TFileStream;
begin
  F := TFileStream.Create(Filename, fmOpenRead or fmShareDenyNone);
  SvgSize := F.Size;
  SetLength(SvgMem, F.Size);
  F.Read(SvgMem[1], F.Size);
  F.Free;

  Bgra := TBgraBitmap.Create(Filename);

  Result := TBitmap.Create;
  Result.PixelFormat := pf24bit;

  Result.SetSize(Bgra.Width, Bgra.Height);
  Result.Canvas.Brush.Color := clWhite;
  Result.Canvas.FillRect(0,0, Result.Width, Result.Height);

  Bgra.Draw(Result.Canvas, 0,0, false);

  Bgra.Free;


  StatusBar1.Panels[1].Text := 'SVG size: ' + IntToStr(Round(SvgSize/1024)) + ' kB';
end;

function TForm1.GetTempName(Ext: String): String;
begin
  Randomize;
  Result := GetTempDir + '/' + IntToStr( Random( High(Int64)) ) + '.' + Ext;
end;

procedure TForm1.BgraToBitmap(Bgra: TBgraBitmap; Bmp: TBitmap);
begin
  Bmp.SetSize(Bgra.Width, Bgra.Height);
  Bgra.Draw(Bmp.Canvas, 0,0, false);
end;

end.

