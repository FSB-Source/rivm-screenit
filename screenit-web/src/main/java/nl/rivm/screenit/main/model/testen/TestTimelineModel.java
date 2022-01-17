package nl.rivm.screenit.main.model.testen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.model.Gemeente;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.topicuszorg.patientregistratie.persoonsgegevens.model.Geslacht;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class TestTimelineModel implements Serializable, IDetachable
{

	private static final long serialVersionUID = 1L;

	private String bsn;

	private Date geboortedatum;

	private IModel<Gemeente> gemeente;

	private String postcode;

	private Geslacht geslacht = Geslacht.MAN;

	private BigDecimal deelnamekans;

	private MammaDoelgroep doelgroep;

	private List<TestTimelineRonde> rondes = new ArrayList<TestTimelineRonde>();

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

	public List<String> getBsns()
	{
		return Arrays.asList(bsn.split(","));
	}

	public String getBsnsString()
	{
		return bsn;
	}

	public void setBsn(String bsn)
	{
		this.bsn = bsn;
	}

	public Date getGeboortedatum()
	{
		return geboortedatum;
	}

	public void setGeboortedatum(Date geboortedatum)
	{
		this.geboortedatum = geboortedatum;
	}

	public Gemeente getGemeente()
	{
		return ModelUtil.nullSafeGet(gemeente);
	}

	public void setGemeente(Gemeente gemeente)
	{
		this.gemeente = ModelUtil.sModel(gemeente);
	}

	public String getPostcode()
	{
		return postcode;
	}

	public void setPostcode(String postcode)
	{
		this.postcode = postcode;
	}

	public Geslacht getGeslacht()
	{
		return geslacht;
	}

	public void setGeslacht(Geslacht geslacht)
	{
		this.geslacht = geslacht;
	}

	public BigDecimal getDeelnamekans()
	{
		return deelnamekans;
	}

	public void setDeelnamekans(BigDecimal deelnamekans)
	{
		this.deelnamekans = deelnamekans;
	}

	public List<TestTimelineRonde> getRondes()
	{
		return rondes;
	}

	public void setRondes(List<TestTimelineRonde> rondes)
	{
		this.rondes = rondes;
	}

	public MammaDoelgroep getDoelgroep()
	{
		return doelgroep;
	}

	public void setDoelgroep(MammaDoelgroep doelgroep)
	{
		this.doelgroep = doelgroep;
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(gemeente);
	}
}
