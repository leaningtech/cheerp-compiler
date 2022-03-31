FROM rockylinux:8

RUN dnf --enablerepo=powertools install -y ninja-build clang lld python3 \
        cmake rpm-build rpm-sign openssh git rsync createrepo autoconf automake \
        libtool
