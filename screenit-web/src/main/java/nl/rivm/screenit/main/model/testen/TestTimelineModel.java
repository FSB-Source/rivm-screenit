package nl.rivm.screenit.main.model.testen;

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

import java.io.Serializable;
import java.math.BigDecimal;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

@Getter
@Setter
public class TestTimelineModel implements Serializable, IDetachable
{

	private String bsn;

	private String aNummer;

	private Date geboortedatum;

	private IModel<Gemeente> gemeente;

	private String postcode;

	private Geslacht geslacht = Geslacht.MAN;

	private BigDecimal deelnamekans;

	private MammaDoelgroep doelgroep;

	private List<TestTimelineRonde> rondes = new ArrayList<>();

	private int leeftijd;

	public TestTimelineModel()
	{
		try
		{
			SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
			geboortedatum = format.parse("01-01-1950");
		}
		catch (ParseException e)
		{

		}

	}

	public Gemeente getGemeente()
	{
		return ModelUtil.nullSafeGet(gemeente);
	}

	public void setGemeente(Gemeente gemeente)
	{
		this.gemeente = ModelUtil.sModel(gemeente);
	}

	public List<String> getBsns()
	{
		return Arrays.asList(bsn.split(","));
	}

	public String getBsnsString()
	{
		return bsn;
	}

	public List<String> getaNummer()
	{
		return Arrays.asList(aNummer.split(","));
	}

	public String getaNummerString()
	{
		return aNummer;
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(gemeente);
	}
}
