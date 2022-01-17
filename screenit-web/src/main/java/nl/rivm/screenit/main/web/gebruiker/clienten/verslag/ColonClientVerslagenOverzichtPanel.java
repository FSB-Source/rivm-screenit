package nl.rivm.screenit.main.web.gebruiker.clienten.verslag;

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

import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.colon.ColonVerslag;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.model.IModel;

public class ColonClientVerslagenOverzichtPanel extends ClientVerslagenOverzichtPanel<ColonVerslag<?>>
{

	private static final long serialVersionUID = 1L;

	public ColonClientVerslagenOverzichtPanel(String id, IModel<Client> model)
	{
		super(id, model);
	}

	@Override
	protected IModel<? extends ColonVerslag<?>> getVerslagFilter()
	{
		IModel<ColonVerslag<?>> model = ModelUtil.cModel(new ColonVerslag<>());
		ColonVerslag<?> verslag = model.getObject();

		verslag.setScreeningRonde(new ColonScreeningRonde());

		ColonScreeningRonde screeningRonde = verslag.getScreeningRonde();
		screeningRonde.setDossier(new ColonDossier());
		ColonDossier dossier = screeningRonde.getDossier();
		dossier.setClient(getModelObject());

		return model;
	}

	@Override
	protected boolean magOverzichtZien()
	{

		List<Bevolkingsonderzoek> bevolkingsonderzoeken = ScreenitSession.get().getOnderzoeken();
		IModel<Client> clientModel = getModel();
		return (bevolkingsonderzoeken.contains(Bevolkingsonderzoek.COLON)
			&& (ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_UITSLAGCOLOSCOPIEONTVANGEN, Actie.INZIEN, clientModel.getObject())
				|| ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CLIENT_SR_UITSLAGPATHOLOGIEONTVANGEN, Actie.INZIEN, clientModel.getObject())));
	}

}
