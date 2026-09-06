{ config, pkgs, ... }: {

  home.packages = with pkgs; [
    zotero
    tesseract
    poppler_utils
  ];

}

