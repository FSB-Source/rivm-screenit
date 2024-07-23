package nl.rivm.screenit.main.service.mamma.impl;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.nio.file.Path;

import lombok.AllArgsConstructor;
import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.service.mamma.MammaDense2Service;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Level;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.logging.LogEvent;
import nl.rivm.screenit.service.LogService;
import nl.rivm.screenit.service.ProjectBestandVerwerkingService;

import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

@Slf4j
@Service
@AllArgsConstructor
public class MammaDense2ServiceImpl implements MammaDense2Service
{
	private final LogService logService;

	private final ProjectBestandVerwerkingService projectBestandVerwerkingService;

	@Override
	public String importClienten(MultipartFile importBestand, InstellingGebruiker instellingGebruiker)
	{
		var convFile = Path.of(System.getProperty("java.io.tmpdir"), importBestand.getOriginalFilename());
		try
		{
			if (!FileType.CSV.getAllowedContentTypes().contains(importBestand.getContentType()))
			{
				throw new IllegalStateException("Bestandstype niet toegestaan.");
			}
			importBestand.transferTo(convFile);
			try (var context = new MammaDense2ImportVerwerkingContext(convFile.toFile()))
			{
				context.init();
				while (context.volgendeRegel())
				{
					projectBestandVerwerkingService.verwerkRegel(context);
				}
				logGebeurtenis(context.heeftMislukt() ? Level.ERROR : Level.INFO, context.getRapportage() + "<br>" + context.getMeldingen(), instellingGebruiker);
				return "Resultaat verwerking: " + context.getRapportage();
			}
		}
		catch (IllegalStateException e)
		{
			LOG.error("Er is een fout opgetreden", e);
			logGebeurtenis(Level.ERROR, e.getMessage(), instellingGebruiker);
		}
		catch (Exception e)
		{
			LOG.error("Er is een onbekende fout opgetreden.", e);
			logGebeurtenis(Level.ERROR, e.getMessage(), instellingGebruiker);
		}
		finally
		{
			convFile.toFile().delete();
		}
		return "Fout bij verwerking bestand.";
	}

	private void logGebeurtenis(Level level, String verslag, InstellingGebruiker instellingGebruiker)
	{
		var event = new LogEvent();
		event.setLevel(level);
		event.setMelding(verslag);
		logService.logGebeurtenis(LogGebeurtenis.MAMMA_DENSE2_IMPORT, event, instellingGebruiker);
	}

}
