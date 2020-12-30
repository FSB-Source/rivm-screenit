package nl.rivm.screenit.main.model.mamma.beoordeling;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.mamma.MammaScreeningsEenheid;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class MammaBaseWerklijstZoekObject implements IDetachable
{
	private Date geboortedatum;

	private String bsn;

	private String postcode;

	private Integer huisnummer;

	private IModel<List<MammaScreeningsEenheid>> screeningsEenheden;

	private List<MammaBeoordelingStatus> beoordelingStatussen = new ArrayList<>();

	public Date getGeboortedatum()
	{
		return geboortedatum;
	}

	public void setGeboortedatum(Date geboortedatum)
	{
		this.geboortedatum = geboortedatum;
	}

	public String getBsn()
	{
		return bsn;
	}

	public void setBsn(String bsn)
	{
		this.bsn = bsn;
	}

	public String getPostcode()
	{
		return postcode;
	}

	public void setPostcode(String postcode)
	{
		this.postcode = postcode;
	}

	public Integer getHuisnummer()
	{
		return huisnummer;
	}

	public void setHuisnummer(Integer huisnummer)
	{
		this.huisnummer = huisnummer;
	}

	public List<MammaScreeningsEenheid> getScreeningsEenheden()
	{
		return ModelUtil.nullSafeGet(screeningsEenheden);
	}

	public void setScreeningsEenheden(List<MammaScreeningsEenheid> screeningsEenheden)
	{
		this.screeningsEenheden = ModelUtil.listRModel(screeningsEenheden);
	}

	public List<MammaBeoordelingStatus> getBeoordelingStatussen()
	{
		return beoordelingStatussen;
	}

	public void setBeoordelingStatussen(List<MammaBeoordelingStatus> beoordelingStatussen)
	{
		this.beoordelingStatussen = beoordelingStatussen;
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(screeningsEenheden);
	}
}
