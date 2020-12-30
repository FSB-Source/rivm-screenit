
package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.zorginstelling;

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

import java.util.List;

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.FqdnPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.KoppelAanParentOrganisatiePanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieBeheer;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatiePaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieZoeken;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.InstellingGebruiker;
import nl.rivm.screenit.model.ZorgInstelling;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.InstellingService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	recht = {
		Recht.GEBRUIKER_ZORGINSTELLING_ORG_BEHEER },
	checkScope = true,
	level = ToegangLevel.INSTELLING,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON, Bevolkingsonderzoek.MAMMA })
public class AanvullendeZiGegevensPage extends OrganisatieBeheer
{

	private static final long serialVersionUID = 1L;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private AutorisatieService autorisatieService;

	public AanvullendeZiGegevensPage()
	{
		Instelling organisatie = getCurrentSelectedOrganisatie();
		add(new OrganisatiePaspoortPanel("paspoort", ModelUtil.sModel(organisatie)));

		final IModel<ZorgInstelling> model = ModelUtil.cModel((ZorgInstelling) organisatie);
		setDefaultModel(model);

		Form<Void> form = new Form<>("form");
		add(form);

		InstellingGebruiker gebruiker = ScreenitSession.get().getLoggedInInstellingGebruiker();
		Actie actie = autorisatieService.getActieVoorOrganisatie(gebruiker, organisatie, Recht.GEBRUIKER_ZORGINSTELLING_ORG_BEHEER);
		List<Bevolkingsonderzoek> bevolkingsonderzoeken = autorisatieService.getBevolkingsonderzoeken(gebruiker);
		boolean inzien = !isMinimumActie(actie, Actie.AANPASSEN);
		boolean gebruikerMagColonZien = bevolkingsonderzoeken.contains(Bevolkingsonderzoek.COLON) && gebruiker.getBevolkingsonderzoeken().contains(Bevolkingsonderzoek.COLON);
		boolean gebruikerMagMammaZien = bevolkingsonderzoeken.contains(Bevolkingsonderzoek.MAMMA) && gebruiker.getBevolkingsonderzoeken().contains(Bevolkingsonderzoek.MAMMA);

		form.add(new KoppelAanParentOrganisatiePanel<ZorgInstelling>("parent", model).setEnabled(!inzien || organisatie.getParent() == null));
		form.add(new GekoppeldePaLabsPanel<ZorgInstelling>("paLabs", model).setVisible(gebruikerMagColonZien));
		form.add(new GekoppeldeColoscopiePanel<ZorgInstelling>("coloscopiesCentrums", model).setVisible(gebruikerMagColonZien));
		form.add(new GekoppeldeMammapoliRadiologiePanel("mammapolisRadiologieAfdelingen", model).setVisible(gebruikerMagMammaZien));
		form.add(new FqdnPanel<ZorgInstelling>("fqdn", model).setVisible(gebruikerMagColonZien));
		form.add(new AjaxSubmitLink("submit")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				BasePage.markeerFormulierenOpgeslagen(target);
				instellingService.saveOrUpdate(model.getObject());
				this.info("Gegevens zijn succesvol opgeslagen");
			}
		});

		AjaxLink<Void> annuleren = new AjaxLink<Void>("annuleren")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(OrganisatieZoeken.class);
			}
		};
		form.add(annuleren);
	}
}
