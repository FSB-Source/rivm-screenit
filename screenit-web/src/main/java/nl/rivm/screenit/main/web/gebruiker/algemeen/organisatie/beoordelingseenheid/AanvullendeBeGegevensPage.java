package nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.beoordelingseenheid;

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

import nl.rivm.screenit.main.service.mamma.MammaScreeningsEenheidService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.KoppelAanParentOrganisatiePanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieBeheer;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatiePaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.algemeen.organisatie.OrganisatieZoeken;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.BeoordelingsEenheid;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.service.AutorisatieService;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_BEOORDELINGSEENHEID_ORG_BEHEER },
	checkScope = true,
	level = ToegangLevel.INSTELLING,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class AanvullendeBeGegevensPage extends OrganisatieBeheer
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private AutorisatieService autorisatieService;

	@SpringBean
	private MammaScreeningsEenheidService screeningsEenheidService;

	@SpringBean
	private LogService logService;

	public AanvullendeBeGegevensPage()
	{
		BeoordelingsEenheid beoordelingsEenheid = (BeoordelingsEenheid) getCurrentSelectedOrganisatie();
		add(new OrganisatiePaspoortPanel("paspoort", ModelUtil.sModel(beoordelingsEenheid)));

		final IModel<BeoordelingsEenheid> model = ModelUtil.cModel(beoordelingsEenheid);
		setDefaultModel(model);

		Form<Void> form = new ScreenitForm<>("form");
		add(form);

		boolean inzien = isAlleenInzien(beoordelingsEenheid);

		form.add(new KoppelAanParentOrganisatiePanel<>("parent", model).setEnabled(!inzien));

		form.add(new Label("screeningseenheden", screeningsEenheidService.getScreeningsEenhedenNamen(beoordelingsEenheid)));

		addOpslaanButton(form, model, inzien);
		addAnnulerenButton(form, inzien);
	}

	private boolean isAlleenInzien(Instelling organisatie)
	{
		Actie actie = autorisatieService.getActieVoorOrganisatie(ScreenitSession.get().getLoggedInInstellingGebruiker(), organisatie,
			Recht.GEBRUIKER_BEOORDELINGSEENHEID_ORG_BEHEER);
		return !isMinimumActie(actie, Actie.AANPASSEN);
	}

	private void addAnnulerenButton(Form<Void> form, boolean inzien)
	{
		AjaxLink<Void> annuleren = new AjaxLink<Void>("annuleren")
		{
			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(OrganisatieZoeken.class);
			}
		};
		annuleren.setVisible(!inzien);
		form.add(annuleren);
	}

	private void addOpslaanButton(Form<Void> form, IModel<BeoordelingsEenheid> model, boolean inzien)
	{
		AjaxSubmitLink opslaan = new AjaxSubmitLink("submit")
		{
			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				BasePage.markeerFormulierenOpgeslagen(target);
				BeoordelingsEenheid beoordelingsEenheid = model.getObject();
				instellingService.saveOrUpdate(beoordelingsEenheid);
				logAction(beoordelingsEenheid);
				this.info("Gegevens zijn succesvol opgeslagen");
			}
		};

		opslaan.setVisible(!inzien);
		form.add(opslaan);
	}

	private void logAction(BeoordelingsEenheid beoordelingsEenheid)
	{
		logService.logGebeurtenis(LogGebeurtenis.ORGANISATIE_WIJZIG, ScreenitSession.get().getLoggedInAccount(),
			"Organisatie: " + beoordelingsEenheid.getNaam() + " ;CE = " + beoordelingsEenheid.getParent().getNaam());
	}
}
