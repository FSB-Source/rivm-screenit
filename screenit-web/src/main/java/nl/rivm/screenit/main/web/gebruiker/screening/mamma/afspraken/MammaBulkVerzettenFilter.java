package nl.rivm.screenit.main.web.gebruiker.screening.mamma.afspraken;

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

import java.time.LocalDate;
import java.util.Date;

import nl.rivm.screenit.dto.mamma.afspraken.IMammaBulkVerzettenFilter;
import nl.rivm.screenit.model.mamma.MammaStandplaatsPeriode;
import nl.rivm.screenit.model.mamma.enums.MammaVerzettenReden;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

class MammaBulkVerzettenFilter implements IMammaBulkVerzettenFilter, IDetachable
{

	private Date vanaf;

	private LocalDate vanafLocalDate;

	private Date totEnMet;

	private LocalDate totEnMetLocalDate;

	private MammaVerzettenReden reden;

	private IModel<MammaStandplaatsPeriode> periodeModel;

	public MammaBulkVerzettenFilter(Date minDag, Date maxDag, IModel<MammaStandplaatsPeriode> periodeModel)
	{
		setVanaf(minDag);
		setTotEnMet(maxDag);
		this.periodeModel = periodeModel;
	}

	@Override
	public Date getVanaf()
	{
		return vanaf;
	}

	@Override
	public LocalDate getVanafLocalDate()
	{
		return vanafLocalDate;
	}

	@Override
	public Date getTotEnMet()
	{
		return totEnMet;
	}

	@Override
	public LocalDate getTotEnMetLocalDate()
	{
		return totEnMetLocalDate;
	}

	@Override
	public MammaStandplaatsPeriode getStandplaatsPeriode()
	{
		return ModelUtil.nullSafeGet(periodeModel);
	}

	@Override
	public void setVanaf(Date vanaf)
	{
		this.vanaf = vanaf;
		this.vanafLocalDate = DateUtil.toLocalDate(vanaf);
	}

	@Override
	public void setTotEnMet(Date totEnMet)
	{
		this.totEnMet = totEnMet;
		this.totEnMetLocalDate = DateUtil.toLocalDate(totEnMet);
	}

	@Override
	public void setStandplaatsPeriode(MammaStandplaatsPeriode periode)
	{
		this.periodeModel = ModelUtil.sModel(periode);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(periodeModel);
	}

	@Override
	public MammaVerzettenReden getVerzettenReden()
	{
		return reden;
	}

	@Override
	public void setVerzettenReden(MammaVerzettenReden reden)
	{
		this.reden = reden;
	}
}
