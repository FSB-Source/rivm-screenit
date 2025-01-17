package nl.rivm.screenit.main.web.gebruiker.algemeen.medewerker;

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

import java.util.List;

import nl.rivm.screenit.main.service.MedewerkerService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieBasisgegevens;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatiemedewerker.OrganisatieMedewerkerKoppelPage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MedewerkerKoppelPage extends OrganisatieMedewerkerKoppelPage
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private MedewerkerService medewerkerService;

	public MedewerkerKoppelPage()
	{
		super(false);
	}

	@Override
	public Actie getActie(Recht recht)
	{
		Actie actie = autorisatieService.getActieVoorOrganisatie(
			ScreenitSession.get().getLoggedInInstellingGebruiker(), 
			getCurrentSelectedOrganisatie(), 
			recht);
		return actie;
	}

	@Override
	protected Panel getPaspoortPanel(String id)
	{
		return new MedewerkerPaspoortPanel(id, ModelUtil.cRModel(getCurrentSelectedMedewerker()));
	}

	@Override
	protected void onNavigeerNaar(IModel<InstellingGebruiker> rowModel, AjaxRequestTarget target)
	{
		Instelling organisatie = rowModel.getObject().getOrganisatie();
		setCurrentSelectedOrganisatie(organisatie);
		setResponsePage(new OrganisatieBasisgegevens(ModelUtil.cModel(organisatie)));
	}

	@Override
	protected boolean magNavigerenNaar(IModel<InstellingGebruiker> rowModel)
	{
		InstellingGebruiker loggedInInstellingGebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		InstellingGebruiker instellingGebruiker = rowModel.getObject();
		Instelling organisatie = instellingGebruiker.getOrganisatie();
		Recht recht = organisatie.getOrganisatieType().getRecht();
		if (recht != null && autorisatieService.getActieVoorOrganisatie(loggedInInstellingGebruiker, organisatie, recht) != null)
		{
			return true;
		}
		return false;
	}

	@Override
	protected InstellingGebruiker createSearchObject()
	{
		InstellingGebruiker searchObject = super.createSearchObject();
		searchObject.setMedewerker(getCurrentSelectedMedewerker());
		return searchObject;
	}

	@Override
	protected void onToevoegen(final IDialog dialog, AjaxRequestTarget target, final WebMarkupContainer medewerkerContainer)
	{
		dialog.setContent(new OrganisatieSmallZoekPanel(IDialog.CONTENT_ID)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void setVorigZoekObject(Instelling organisatieSearchObject)
			{
			}

			@Override
			protected Instelling getVorigZoekObject()
			{
				return null;
			}

			@Override
			protected void onCloseWithSelected(AjaxRequestTarget target, IModel<Instelling> model)
			{
				Gebruiker medewerker = getCurrentSelectedMedewerker();
				Gebruiker loggedInMedewerker = ScreenitSession.get().getLoggedInInstellingGebruiker().getMedewerker();
				if (medewerker.equals(loggedInMedewerker))
				{
					medewerker = loggedInMedewerker;
				}
				Instelling organisatie = ModelUtil.nullSafeGet(model);
				if (organisatie != null)
				{

					medewerkerService.addOrganisatieMedewerker(organisatie, medewerker);
					logService.logGebeurtenis(LogGebeurtenis.ORGANISATIE_MEDEWERKER_KOPPEL, ScreenitSession.get().getLoggedInInstellingGebruiker(),
						String.format("Medewerker %1$s gekoppeld aan organisatie %2$s", medewerker.getNaamVolledig(), organisatie.getNaam()));
					target.add(medewerkerContainer);
				}
				dialog.close(target);
			}
		});
		dialog.open(target);
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		return MedewerkerBeheer.createContextMenu();
	}
}
