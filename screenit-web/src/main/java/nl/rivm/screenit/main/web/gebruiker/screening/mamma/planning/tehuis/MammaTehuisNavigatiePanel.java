package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.tehuis;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientInzienPage;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.enums.Deelnamemodus;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.service.mamma.MammaBaseDossierService;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaTehuisNavigatiePanel extends GenericPanel<Client>
{
	@SpringBean
	private MammaBaseDossierService baseDossierService;

	public MammaTehuisNavigatiePanel(String id, IModel<Client> model)
	{
		super(id, model);

		add(new IndicatingAjaxLink<Void>("dossierOpenen")
		{
			@Override
			public boolean isVisible()
			{
				return ScreenitSession.get().getAuthorizationStrategy().isInstantiationAuthorized(ClientInzienPage.class);
			}

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(new ClientInzienPage(new SimpleHibernateModel<>(model.getObject())));
			}
		});
		add(new MammaNaarAfspraakMakenPanel("afspraakMaken", model)
		{
			@Override
			public boolean isVisible()
			{
				MammaDossier dossier = model.getObject().getMammaDossier();
				return dossier.getDeelnamemodus() != Deelnamemodus.SELECTIEBLOKKADE &&
					(baseDossierService.isAfspraakMakenMogelijk(dossier, false, false) || baseDossierService.isVerzettenMogelijk(dossier));
			}
		});
	}
}
