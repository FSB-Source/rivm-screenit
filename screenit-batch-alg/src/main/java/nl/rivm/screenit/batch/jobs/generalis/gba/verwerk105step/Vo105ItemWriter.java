
package nl.rivm.screenit.batch.jobs.generalis.gba.verwerk105step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
 * %%
 * Copyright (C) 2012 - 2020 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import nl.rivm.screenit.batch.jobs.generalis.gba.wrappers.Vo105BerichtWrapper;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.topicuszorg.gba.vertrouwdverbonden.model.utils.VoxHelper;

import org.apache.commons.io.IOUtils;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.ItemStreamWriter;
import org.springframework.batch.item.ItemWriter;

public class Vo105ItemWriter implements ItemWriter<List<Vo105BerichtWrapper>>, ItemStreamWriter<List<Vo105BerichtWrapper>>
{
	
	public static final String BESTANDEN_MAP_KEY = "key.bestandenmap";

	private Map<String, String> vo105Bestanden = new HashMap<>();

	private String voFileStorePath;

	@Override
	public void write(List<? extends List<Vo105BerichtWrapper>> items) throws IOException
	{
		for (List<Vo105BerichtWrapper> vo105Berichten : items)
		{
			for (Vo105BerichtWrapper vo105Bericht : vo105Berichten)
			{
				writeBericht(vo105Bericht);
			}
		}
	}

	private void writeBericht(Vo105BerichtWrapper vo105Bericht) throws IOException
	{
		ScreeningOrganisatie screeningOrganisatie = vo105Bericht.getScreeningOrganisatie();
		if (!vo105Bestanden.containsKey(screeningOrganisatie.getRegioCode()))
		{
			File dir = new File(voFileStorePath + System.getProperty("file.separator") + "temp");
			dir.mkdirs();
			vo105Bestanden.put(screeningOrganisatie.getRegioCode(), File.createTempFile("so" + screeningOrganisatie.getRegioCode(), "", dir).getPath());
		}

		File file = new File(vo105Bestanden.get(screeningOrganisatie.getRegioCode()));
		FileWriter fileWriter = null;
		try
		{
			fileWriter = new FileWriter(file, true);
			IOUtils.write(VoxHelper.convertToBerichtString(vo105Bericht.getVo105Bericht()), fileWriter);
		}
		finally
		{
			if (fileWriter != null)
			{
				fileWriter.close();
			}
		}
	}

	@Override
	public void open(ExecutionContext executionContext) throws ItemStreamException
	{
		if (executionContext.containsKey(BESTANDEN_MAP_KEY))
		{
			this.vo105Bestanden = (Map<String, String>) executionContext.get(BESTANDEN_MAP_KEY);
		}
		else
		{
			vo105Bestanden.clear();
		}
	}

	@Override
	public void update(ExecutionContext executionContext) throws ItemStreamException
	{
		executionContext.put(BESTANDEN_MAP_KEY, new HashMap<>(vo105Bestanden));
	}

	@Override
	public void close() throws ItemStreamException
	{
		vo105Bestanden.clear();
	}

	public void setVoFileStorePath(String voFileStorePath)
	{
		this.voFileStorePath = voFileStorePath;
	}
}
