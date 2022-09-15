ubuntu:
	sudo apt update; \
        sudo apt upgrade -y; \
	sudo add-apt-repository -y ppa:ubuntu-toolchain-r/ppa ;\
	sudo apt install -y gcc-10 g++-10 libgccjit0 libgccjit-10-dev libjansson4 libjansson-dev \
				build-essential libgtk-3-dev libgnutls28-dev libtiff5-dev libgif-dev \
				libjpeg-dev libpng-dev libxpm-dev libncurses-dev libtool-bin texinfo cmake \
				automake libncurses5-dev g++ unzip	
conda:
	wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh ;\
        sh Miniconda3-latest-Linux-x86_64.sh -b ;\
        $(HOME)/miniconda3/bin/conda init $(SHELL);\
        $(HOME)/miniconda3/bin/conda config --add channels conda-forge ;\
        $(HOME)/miniconda3/bin/conda config --add channels litex-hub ;\

conda-base:
	conda install -y mamba ;\

emacs:
	git clone git://git.savannah.gnu.org/emacs.git ;\
	cd emacs ;\
#	git checkout emacs-28 ;\
	./autogen ;\
	./configure --with-pgtk --with-mail-utils --with-native-compilation ;\


neovim: 
	git clone https://github.com/neovim/neovim.git ;\
	cd neovim ;\
	make cmake && make test 

gh-cli:
	curl -fsSL https://cli.github.com/packages/githubcli-archive-keyring.gpg |  sudo dd of=/usr/share/keyrings/githubcli-archive-keyring.gpg ;\
        sudo chmod go+r /usr/share/keyrings/githubcli-archive-keyring.gpg ;\
	echo "deb [arch=$$(dpkg --print-architecture) signed-by=/usr/share/keyrings/githubcli-archive-keyring.gpg] https://cli.github.com/packages stable main" | sudo tee /etc/apt/sources.list.d/github-cli.list > /dev/null ;\
        sudo apt update ;\
        sudo apt install -y gh ;\
	git config --global user.email "ahmmo@gmail.com" ;\
	git config --global user.name  "Monib Ahmed" 

home:
	ln -s ./.emacs ~/.
	ln -s ./.zshrc ~/.

