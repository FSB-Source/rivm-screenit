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

import java.util.Date;
import java.util.List;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.mamma.enums.MammobridgeRole;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class MammaPortfolioZoekObject implements IDetachable
{
	private IModel<List<InstellingGebruiker>> instellingGebruikers;

	private Date vanaf;

	private Date totEnMet;

	private MammobridgeRole mammobridgeRol;

	public List<InstellingGebruiker> getInstellingGebruikers()
	{
		return ModelUtil.nullSafeGet(instellingGebruikers);
	}

	public void setInstellingGebruikers(List<InstellingGebruiker> instellingGebruikers)
	{
		this.instellingGebruikers = ModelUtil.listRModel(instellingGebruikers);
	}

	public Date getVanaf()
	{
		return vanaf;
	}

	public void setVanaf(Date vanaf)
	{
		this.vanaf = vanaf;
	}

	public Date getTotEnMet()
	{
		return totEnMet;
	}

	public void setTotEnMet(Date totEnMet)
	{
		this.totEnMet = totEnMet;
	}

	public MammobridgeRole getMammobridgeRol()
	{
		return mammobridgeRol;
	}

	public void setMammobridgeRol(MammobridgeRole mammobridgeRol)
	{
		this.mammobridgeRol = mammobridgeRol;
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(instellingGebruikers);
	}
}
