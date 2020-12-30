package nl.rivm.screenit.util.cervix.HpvBerichtGenerator;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.Date;

public class CervixHpvBerichtGeneratorMonsterWrapper implements Serializable
{

	private static final long serialVersionUID = 1L;

	private String barcode;

	private CervixHpvBerichtWaarde uitslag;

	private Date autorisatieDatum;

	private Date analyseDatum;

	private CervixHpvBerichtGeneratorWrapper wrapper;

	public String getBarcode()
	{
		return barcode;
	}

	public void setBarcode(String barcode)
	{
		this.barcode = barcode;
	}

	public CervixHpvBerichtWaarde getUitslag()
	{
		return uitslag;
	}

	public void setUitslag(CervixHpvBerichtWaarde uitslag)
	{
		this.uitslag = uitslag;
	}

	public Date getAutorisatieDatum()
	{
		return autorisatieDatum;
	}

	public void setAutorisatieDatum(Date autorisatieDatum)
	{
		this.autorisatieDatum = autorisatieDatum;
	}

	public String getStringAutorisatieDatum()
	{
		SimpleDateFormat format = new SimpleDateFormat("yyyyMMddhhmmss");
		return format.format(autorisatieDatum);
	}

	public Date getAnalyseDatum()
	{
		return analyseDatum;
	}

	public String getStringAnalyseDatum()
	{
		SimpleDateFormat format = new SimpleDateFormat("yyyyMMddhhmmss");
		return format.format(analyseDatum);
	}

	public void setAnalyseDatum(Date analyseDatum)
	{
		this.analyseDatum = analyseDatum;
	}

	public CervixHpvBerichtGeneratorWrapper getWrapper()
	{
		return wrapper;
	}

	public void setWrapper(CervixHpvBerichtGeneratorWrapper wrapper)
	{
		this.wrapper = wrapper;
	}

}
