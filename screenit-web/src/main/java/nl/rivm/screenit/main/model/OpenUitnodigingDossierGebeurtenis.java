package nl.rivm.screenit.main.model;

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

import java.util.Date;

import nl.rivm.screenit.model.colon.OpenUitnodiging;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class OpenUitnodigingDossierGebeurtenis extends DossierGebeurtenis implements IDetachable
{

	private static final long serialVersionUID = 1L;

	private String omschrijving;

	private IModel<OpenUitnodiging> openUitnodigingModel;

	public OpenUitnodigingDossierGebeurtenis(Date tijd)
	{
		super(tijd);
	}

	public String getOmschrijving()
	{
		return omschrijving;
	}

	public void setOmschrijving(String omschrijving)
	{
		this.omschrijving = omschrijving;
	}

	public OpenUitnodiging getOpenUitnodiging()
	{
		return ModelUtil.nullSafeGet(openUitnodigingModel);
	}

	public IModel<OpenUitnodiging> getOpenUitnodigingModel()
	{
		return openUitnodigingModel;
	}

	public void setOpenUitnodiging(OpenUitnodiging uitnodiging)
	{
		this.openUitnodigingModel = ModelUtil.sModel(uitnodiging);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(openUitnodigingModel);
	}

}
