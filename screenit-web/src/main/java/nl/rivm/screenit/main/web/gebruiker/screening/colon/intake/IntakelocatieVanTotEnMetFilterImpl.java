package nl.rivm.screenit.main.web.gebruiker.screening.colon.intake;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import java.util.Date;

import nl.rivm.screenit.dao.colon.IntakelocatieVanTotEnMetFilter;
import nl.rivm.screenit.model.colon.ColonIntakelocatie;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class IntakelocatieVanTotEnMetFilterImpl implements IntakelocatieVanTotEnMetFilter, IDetachable
{

	private static final long serialVersionUID = 1L;

	private IModel<ColonIntakelocatie> intakelocatie;

	private Date vanaf;

	private Date totEnMet;

	private String bsn;

	public IntakelocatieVanTotEnMetFilterImpl(IModel<ColonIntakelocatie> intakelocatieModel, Date vanaf, Date totEnMet)
	{
		this.intakelocatie = intakelocatieModel;
		this.vanaf = vanaf;
		this.totEnMet = totEnMet;
	}

	@Override
	public ColonIntakelocatie getIntakelocatie()
	{
		return ModelUtil.nullSafeGet(intakelocatie);
	}

	@Override
	public void setIntakelocatie(ColonIntakelocatie intakelocatie)
	{
		this.intakelocatie = ModelUtil.sModel(intakelocatie);
	}

	@Override
	public Date getVanaf()
	{
		return vanaf;
	}

	@Override
	public void setVanaf(Date vanaf)
	{
		this.vanaf = vanaf;
	}

	@Override
	public Date getTotEnMet()
	{
		return totEnMet;
	}

	@Override
	public void setTotEnMet(Date totEnMet)
	{
		this.totEnMet = totEnMet;
	}

	@Override
	public String getBsn()
	{
		return bsn;
	}

	@Override
	public void setBsn(String bsn)
	{
		this.bsn = bsn;
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(intakelocatie);
	}
}
