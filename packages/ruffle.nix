{ alsa-lib
, fetchFromGitHub
, makeWrapper
, openssl
, pkg-config
, python3
, rustPlatform
, lib
, wayland
, xorg
, vulkan-loader
, jre_minimal
, cairo
, gtk3
, wrapGAppsHook
, gsettings-desktop-schemas
, glib
}:

rustPlatform.buildRustPackage rec {
  pname = "ruffle";
  version = "nightly-2023-11-03";

  src = fetchFromGitHub {
    owner = "ruffle-rs";
    repo = pname;
    rev = version;
    sha256 = "sha256-a5368pDyMldPIc00siWZMUnBOMxObuYTmkqBIhqfpTI=";
  };

  nativeBuildInputs = [
    glib
    gsettings-desktop-schemas
    jre_minimal
    makeWrapper
    pkg-config
    python3
    wrapGAppsHook
 ];

  buildInputs = [
    alsa-lib
    cairo
    gtk3
    openssl
    wayland
    xorg.libX11
    xorg.libXcursor
    xorg.libXrandr
    xorg.libXi
    xorg.libxcb
    xorg.libXrender
    vulkan-loader
  ];

  dontWrapGApps = true;

  postFixup = ''
    # This name is too generic
    mv $out/bin/exporter $out/bin/ruffle_exporter

    vulkanWrapperArgs+=(
      --prefix LD_LIBRARY_PATH ':' ${vulkan-loader}/lib
    )

    wrapProgram $out/bin/ruffle_exporter \
      "''${vulkanWrapperArgs[@]}"

    wrapProgram $out/bin/ruffle_desktop \
      "''${vulkanWrapperArgs[@]}" \
      "''${gappsWrapperArgs[@]}"
  '';

  cargoBuildFlags = [ "--workspace" ];

  cargoLock = {
    lockFile = ./ruffle-cargo.lock;
    outputHashes = {
      "flash-lso-0.6.0"           = "sha256-kwq9QDAbCf65OlH8RmCmlIK73rHZKMzDPFWrKpkXLto=";
      "gc-arena-0.3.3"            = "sha256-qwyqBIg/FWW0VwmXfBTuCiuPLBepbnoNef6WoEgET4Y=";
      "h263-rs-0.1.0"             = "sha256-Akf1SBjo8qikhiHI8NPvO3vJvVfm0dQBf2X9V7OdgQc=";
	  "jpegxr-0.3.0"              = "sha256-IZEfuDAO3eWhoz/lYljNQLfS6f7vwMLJYg1BliYm8fo=";
      "nellymoser-rs-0.1.2"       = "sha256-GykDQc1XwySOqfxW/OcSxkKCFJyVmwSLy/CEBcwcZJs=";
      "nihav_codec_support-0.1.0" = "sha256-HAJS4I6yyzQzCf+vmaFp1MWXpcUgFAHPxLhfMVXmN1c=";
	  "wgpu-0.17.1"               = "sha256-Emmn2p8l6na8SIpjSpmeGCKgWbrWTnzZlIvCzXVWii4=";
    };
  };

  meta = with lib; {
    description = "An Adobe Flash Player emulator written in the Rust programming language.";
    homepage = "https://ruffle.rs/";
    license = with licenses; [ mit asl20 ];
    maintainers = with maintainers; [ govanify ];
    platforms = platforms.linux;
  };
}
