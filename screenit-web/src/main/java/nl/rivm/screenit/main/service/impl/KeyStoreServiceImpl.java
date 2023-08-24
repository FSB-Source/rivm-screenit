package nl.rivm.screenit.main.service.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.security.GeneralSecurityException;
import java.security.Key;
import java.security.KeyStore;

import nl.rivm.screenit.main.service.KeyStoreService;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;

@Service
@Transactional(propagation = Propagation.SUPPORTS, readOnly = true)
public class KeyStoreServiceImpl implements KeyStoreService
{

	@Autowired
	@Qualifier("locatieFilestore")
	private String locatieFilestore;

	@Override
	public Key getFirstKeyFromKeyStore(String keyStoreLocationOnFilestore, String keyStorePassword, String keyPassword)
	{
		var keyStoreLocation = locatieFilestore + File.separator + keyStoreLocationOnFilestore;

		try (final FileInputStream is = new FileInputStream(keyStoreLocation))
		{
			var keyStore = KeyStore.getInstance(KeyStore.getDefaultType());
			keyStore.load(is, keyStorePassword.toCharArray());
			return keyStore.getKey(keyStore.aliases().nextElement(), keyPassword.toCharArray());
		}
		catch (IOException | GeneralSecurityException e)
		{
			throw new IllegalStateException("Kon key niet uit keystore laden uit keystore: " + keyStoreLocation, e);
		}
	}
}
