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

import nl.rivm.screenit.model.Afmelding;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IDetachable;
import org.apache.wicket.model.IModel;

public class AfmeldenDossierGebeurtenis<A extends Afmelding> extends DossierGebeurtenis implements IDetachable
{

	private static final long serialVersionUID = 1L;

	private IModel<A> afmeldingModel;

	private IModel<A> heraanmeldingModel;

	public AfmeldenDossierGebeurtenis(Date tijd)
	{
		super(tijd);
	}

	public AfmeldenDossierGebeurtenis()
	{
		super();
	}

	public A getAfmelding()
	{
		return ModelUtil.nullSafeGet(afmeldingModel);
	}

	public IModel<A> getAfmeldingModel()
	{
		return afmeldingModel;
	}

	public void setAfmelding(A afmelding)
	{
		this.afmeldingModel = ModelUtil.sModel(afmelding);
	}

	public void setAfmeldingModel(IModel<A> afmeldingModel)
	{
		this.afmeldingModel = afmeldingModel;
	}

	public A getHeraanmelding()
	{
		return ModelUtil.nullSafeGet(heraanmeldingModel);
	}

	public void setHeraanmeldingModel(IModel<A> heraanmeldingModel)
	{
		this.heraanmeldingModel = heraanmeldingModel;
	}

	public IModel<A> getHeraanmeldingModel()
	{
		return heraanmeldingModel;
	}

	public void setHeraanmelding(A heraanmelding)
	{
		this.heraanmeldingModel = ModelUtil.sModel(heraanmelding);
	}

	@Override
	public void detach()
	{
		ModelUtil.nullSafeDetach(afmeldingModel);
		ModelUtil.nullSafeDetach(heraanmeldingModel);
	}

}
