{pkgs, ...}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    # nix tools
    alejandra

    # ruby stuff
    ruby
    rubyPackages.jekyll
  ];
}
