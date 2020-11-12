
package nl.rivm.screenit.batch.jobs.colon.ifobtinlezen.inlezenstep;

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

public class IFOBTCsvReader extends BaseCsvFileReader<IFOBTResult>
{

	@Override
	protected IFOBTResult parseLine(String[] line, int regelNummer, String bestandsNaam) throws ParseException, IllegalStateException
	{
		IFOBTResult result = null;
		if (line.length >= 9)
		{
			DateFormat dateFormat = new SimpleDateFormat("yyyyMMddHHmmss");
			result = new IFOBTResult();

			result.setLabID(nullCheckColumn(line[0]));
			result.setInstrumentID(line[1]);
			result.setSid(nullCheckColumn(line[2]));
			result.setResultValue(nullCheckColumn(line[6]));
			result.setDateTimeResult(dateFormat.parse(nullCheckColumn(line[8]).trim()));
			result.setBestandsNaam(bestandsNaam);
		}
		else
		{
			throw new IllegalStateException("Aantal kolommen is kleiner dan 9");
		}
		return result;
	}
}
