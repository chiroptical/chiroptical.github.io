{pkgs, ...}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    # nix tools
    alejandra

    # ruby stuff
    ruby_3_4
    rubyPackages_3_4.jekyll
  ];
}
