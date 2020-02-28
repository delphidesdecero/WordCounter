program WordCounter;

uses
  System.StartUpCopy,
  FMX.Forms,
  UConfig in 'UConfig.pas' {FConfig},
  UAbout in 'UAbout.pas' {FAbout},
  UPrincipal in 'UPrincipal.pas' {FPrincipal},
  USplash in 'USplash.pas' {FSplash},
  UKeywordsMining in 'UKeywordsMining.pas' {FKeywordMining};

{$R *.res}

begin
  FSplash := TFSplash.Create(Application);
  FSplash.Show;

  Application.Initialize;
  Application.CreateForm(TFPrincipal, FPrincipal);
  Application.CreateForm(TFConfig, FConfig);
  Application.CreateForm(TFAbout, FAbout);
  Application.CreateForm(TFKeywordMining, FKeywordMining);

  while not FSplash.vCompletado do
      Application.ProcessMessages;

  FSplash.Hide;
  FSplash.Free;

  Application.Run;
end.
