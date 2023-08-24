package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen.mamma;

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

import nl.rivm.screenit.main.model.ScreeningRondeGebeurtenis;
import nl.rivm.screenit.model.mamma.MammaFollowUpRadiologieVerslag;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import org.apache.wicket.model.IModel;

public class MammaFollowUpRadiologieVerslagGebeurtenis extends ScreeningRondeGebeurtenis
{
	private final IModel<MammaFollowUpRadiologieVerslag> followUpRadiologieVerslagModel;

	public MammaFollowUpRadiologieVerslagGebeurtenis(MammaFollowUpRadiologieVerslag followUpRadiologieVerslag)
	{
		this.followUpRadiologieVerslagModel = ModelUtil.sModel(followUpRadiologieVerslag);
	}

	public MammaFollowUpRadiologieVerslag getFollowUpRadiologieVerslag()
	{
		return ModelUtil.nullSafeGet(followUpRadiologieVerslagModel);
	}

	@Override
	public void detach()
	{
		super.detach();
		ModelUtil.nullSafeDetach(followUpRadiologieVerslagModel);
	}
}
