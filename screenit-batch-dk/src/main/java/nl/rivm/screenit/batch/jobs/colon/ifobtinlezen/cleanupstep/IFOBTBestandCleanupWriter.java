
package nl.rivm.screenit.batch.jobs.colon.ifobtinlezen.cleanupstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.model.colon.IFOBTBestand;
import nl.rivm.screenit.model.colon.enums.IFOBTBestandStatus;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;

import org.apache.commons.io.FileUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;

public class IFOBTBestandCleanupWriter implements ItemWriter<IFOBTBestand>
{

	private static final Logger LOG = LoggerFactory.getLogger(IFOBTBestandCleanupWriter.class);

	@Autowired
	private HibernateService hibernateService;

	@Autowired
	@Qualifier("ifobtFileLocation")
	private String ifobtFileLocation;

	@Autowired
	@Qualifier("eikenFileLocation")
	private String eikenFileLocation;

	@Override
	public void write(List<? extends IFOBTBestand> bestanden) throws Exception
	{

		List<IFOBTBestand> bestandenVerwijderenVanIfobtLocation = new ArrayList<>();
		for (IFOBTBestand bestand : bestanden)
		{
			if (IFOBTBestandStatus.NIEUW.equals(bestand.getStatus()))
			{
				bestand.setStatus(IFOBTBestandStatus.NIET_VOLLEDIG_INGELEZEN);
				hibernateService.saveOrUpdate(bestand);
			}
			else
			{
				bestandenVerwijderenVanIfobtLocation.add(bestand);
			}
		}
		File ifobtFileLocationDir = new File(ifobtFileLocation);
		for (File ifobtFile : ifobtFileLocationDir.listFiles())
		{
			if (!ifobtFile.isDirectory() && !ifobtFile.getName().startsWith("."))
			{
				for (IFOBTBestand bestand : bestandenVerwijderenVanIfobtLocation)
				{
					if (verwijderFile(ifobtFile, bestand))
					{
						break;
					}
				}
			}
		}

		File eikenFileLocationDir = new File(eikenFileLocation);
		for (File eikenFile : eikenFileLocationDir.listFiles())
		{
			if (!eikenFile.isDirectory() && !eikenFile.getName().startsWith("."))
			{
				for (IFOBTBestand bestand : bestandenVerwijderenVanIfobtLocation)
				{
					if (verwijderFile(eikenFile, bestand))
					{
						break;
					}
				}
			}
		}
	}

	private boolean verwijderFile(File ifobtFile, IFOBTBestand bestand)
	{
		if (ifobtFile.getName().equals(bestand.getNaamBestand()))
		{
			LOG.info("Bestand " + bestand.getNaamBestand() + " volledig ingelezen, mag nu worden verwijderd.");
			FileUtils.deleteQuietly(ifobtFile);

			bestand.setStatus(IFOBTBestandStatus.INGELEZEN);
			hibernateService.saveOrUpdate(bestand);

			return true;
		}
		return false;
	}
}
