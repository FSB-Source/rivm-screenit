package nl.rivm.screenit.batch.jobs.generalis.gba.verwerk105step;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-alg
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.List;

import nl.topicuszorg.gba.vertrouwdverbonden.model.Vo105Bericht;
import nl.topicuszorg.gba.vertrouwdverbonden.model.utils.VoxHelper;

import org.apache.commons.io.IOUtils;
import org.springframework.batch.item.ExecutionContext;
import org.springframework.batch.item.ItemStreamException;
import org.springframework.batch.item.ItemStreamWriter;
import org.springframework.batch.item.ItemWriter;
import org.springframework.stereotype.Component;

@Component
public class Vo105ItemWriter implements ItemWriter<Vo105Bericht>, ItemStreamWriter<Vo105Bericht>
{
	public static final String VO105_BESTAND_KEY = "key.vo105bestand";

	private String vo105FilePath;

	@Override
	public void write(List<? extends Vo105Bericht> berichten) throws IOException
	{
		for (var vo105Bericht : berichten)
		{
			writeBericht(vo105Bericht);
		}
	}

	private void writeBericht(Vo105Bericht vo105Bericht) throws IOException
	{
		if (vo105FilePath == null)
		{
			vo105FilePath = File.createTempFile("VO105_BVO", "").getPath();
		}

		var berichtString = VoxHelper.convertToBerichtString(vo105Bericht);

		try (var fileWriter = new FileWriter(vo105FilePath, true))
		{
			IOUtils.write(berichtString, fileWriter);
		}
	}

	@Override
	public void open(ExecutionContext executionContext) throws ItemStreamException
	{
		if (executionContext.containsKey(VO105_BESTAND_KEY))
		{
			vo105FilePath = (String) executionContext.get(VO105_BESTAND_KEY);
		}
		else
		{
			vo105FilePath = null;
		}
	}

	@Override
	public void update(ExecutionContext executionContext) throws ItemStreamException
	{
		executionContext.put(VO105_BESTAND_KEY, vo105FilePath);
	}

	@Override
	public void close() throws ItemStreamException
	{
		vo105FilePath = null;
	}
}
