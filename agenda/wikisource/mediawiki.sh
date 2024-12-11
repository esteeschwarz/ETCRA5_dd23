wget https://releases.wikimedia.org/mediawiki/1.43/mediawiki-core-1.43.0-rc.0.zip
tar -xvzf mediawiki-core-1.43.0-rc.0.tar.gz
sudo chown -R www-data:www-data /var/www/html/mediawiki
sudo chmod -R 755 /var/www/html/mediawiki
sudo nano /etc/apache2/sites-available/mediawiki.conf
sudo a2ensite mediawiki.conf
sudo a2enmod rewrite
sudo systemctl restart apache2
CREATE DATABASE mediawiki;
CREATE USER 'wikiuser'@'localhost' IDENTIFIED BY 'password';
GRANT ALL PRIVILEGES ON mediawiki.* TO 'wikiuser'@'localhost';
FLUSH PRIVILEGES;
EXIT;