unit FMCommonImages;

interface

implementation

{$R FMCommonImages.RES}

uses IWServer;

initialization
  TIWServer.AddInternalFile('IW_GFX_shade_inactive', '/gfx/shade_inactive.png');
end.
