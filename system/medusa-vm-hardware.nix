{ config, lib, pkgs, modulesPath, ... }:
{
	imports = [(modulesPath + "/profiles/qemu-guest.nix")];

	boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "virtio_pci" "sr_mod" "virtio_blk" ];
	boot.initrd.kernelModules = [];

	boot.kernelModules = [ "kvm-amd" ];
	boot.extraModulePackages = [];

	fileSystems."/" = {
		device = "/dev/disk/by-uuid/dbb337ce-3809-403b-a5d6-44f532a77269";
		fsType = "ext4";
	};

	swapDevices = [];

	nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
}
