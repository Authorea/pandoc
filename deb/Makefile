COMMIT?=HEAD
VAGRANTBOX?=ubuntu/trusty64

.PHONY: package clean

package:
	VAGRANTBOX=$(VAGRANTBOX) vagrant up
	vagrant ssh -c '\
		echo "deb http://download.fpcomplete.com/ubuntu trusty main"|sudo tee /etc/apt/sources.list.d/fpco.list &&\
		sudo apt-get update && sudo apt-get install git stack -y --force-yes &&\
		rm -rf pandoc && git clone https://github.com/authorea/pandoc && cd pandoc && git checkout -b work $(COMMIT) && git submodule update --init && ls -l && ./make_deb.sh && cp *.deb /vagrant_data/'
	vagrant halt

clean:
	vagrant destroy
