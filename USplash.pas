unit USplash;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs;

type
  TFSplash = class(TForm)
    tmrSplash: TTimer;
    procedure tmrSplashTimer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FSplash: TFSplash;

implementation

{$R *.fmx}

uses UPrincipal;

procedure TFSplash.tmrSplashTimer(Sender: TObject);
begin
  FPrincipal.Show;
  Visible := False;
end;

end.
