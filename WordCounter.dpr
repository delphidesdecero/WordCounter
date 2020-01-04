program WordCounter;

uses
  System.StartUpCopy,
  FMX.Forms,
  UPrincipal in 'UPrincipal.pas' {FPrincipal},
  USplash in 'USplash.pas' {FSplash};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFSplash, FSplash);
  Application.CreateForm(TFPrincipal, FPrincipal);
  Application.Run;
end.
