
package nl.rivm.screenit.batch.jobs.colon.ifobtinlezen.eikeninlezenstep;

/*-
 * ========================LICENSE_START=================================
 * screenit-batch-dk
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

import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;

import nl.rivm.screenit.batch.BaseCsvFileReader;
import nl.rivm.screenit.model.colon.IFOBTResult;

public class EikenCsvReader extends BaseCsvFileReader<IFOBTResult>
{

	@Override
	protected IFOBTResult parseLine(String[] line, int regelNummer, String bestandsNaam) throws ParseException, IllegalStateException
	{
		IFOBTResult result = null;
		if (line.length >= 17)
		{
			DateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd hh:mm");
			result = new IFOBTResult();

			result.setLabID("Rot1");

			result.setInstrumentID("00738");

			result.setSid(nullCheckColumn(line[5]));

			result.setResultValue(line[11]);

			result.setDateTimeResult(dateFormat.parse(nullCheckColumn(line[1]).trim() + " " + nullCheckColumn(line[2]).trim()));

			result.setBestandsNaam(bestandsNaam);
		}
		else
		{
			throw new IllegalStateException("Regel heeft niet minimaal 17 kolommen (gescheiden door ',')");
		}
		return result;
	}
}
