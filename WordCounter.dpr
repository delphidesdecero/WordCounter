program WordCounter;

uses
  System.StartUpCopy,
  FMX.Forms,
  UConfig in 'UConfig.pas' {Form1},
  UAbout in 'UAbout.pas' {FAbout},
  UPrincipal in 'UPrincipal.pas' {FPrincipal},
  USplash in 'USplash.pas' {FSplash};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFSplash, FSplash);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFAbout, FAbout);
  Application.CreateForm(TFPrincipal, FPrincipal);
  Application.Run;
end.
