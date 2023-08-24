package nl.rivm.screenit.main.model.mamma.beoordeling;

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

import java.util.Date;

import lombok.Getter;
import lombok.Setter;

import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.MammaConclusieReviewFilterOptie;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

@Getter
@Setter
public class MammaConclusieReviewZoekObject implements IDetachable
{
	private IModel<InstellingGebruiker> instellingGebruiker;

	private MammaConclusieReviewFilterOptie filterOptie = MammaConclusieReviewFilterOptie.ALLES;

	private Boolean gezienTonen = false;

	private Boolean voorDashboard = false;

	private Date zoekenVanafEindconclusieDatum;

	public InstellingGebruiker getInstellingGebruiker()
	{
		return ModelUtil.nullSafeGet(instellingGebruiker);
	}

	public void setInstellingGebruiker(InstellingGebruiker instellingGebruiker)
	{
		this.instellingGebruiker = ModelUtil.sModel(instellingGebruiker);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(instellingGebruiker);
	}
}
