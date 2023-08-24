package nl.rivm.screenit.util.cervix.hpv_berichtgenerator;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Date;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultValue;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvOrderCode;
import nl.rivm.screenit.model.cervix.berichten.CervixHpvResultCode;

@Getter
@Setter
public class CervixHpvBerichtGeneratorMonsterWrapper implements Serializable
{
	private String barcode;

	private CervixHpvOrderCode ordercode;

	private CervixHpvResultCode analysecode1;

	private CervixHpvResultCode analysecode2;

	private CervixHpvResultCode analysecode3;

	private CervixHpvResultValue analyseresultaat1;

	private CervixHpvResultValue analyseresultaat2;

	private CervixHpvResultValue analyseresultaat3;

	private Date autorisatieDatum;

	private Date analyseDatum;

	public String getStringAutorisatieDatum()
	{
		SimpleDateFormat format = new SimpleDateFormat("yyyyMMddhhmmss");
		return format.format(autorisatieDatum);
	}

	public String getStringAnalyseDatum()
	{
		SimpleDateFormat format = new SimpleDateFormat("yyyyMMddhhmmss");
		return format.format(analyseDatum);
	}
}
