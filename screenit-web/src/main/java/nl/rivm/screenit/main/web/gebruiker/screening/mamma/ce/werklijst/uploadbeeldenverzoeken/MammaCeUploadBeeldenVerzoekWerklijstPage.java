package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.uploadbeeldenverzoeken;

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

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import nl.rivm.screenit.dto.mamma.MammaUploadBeeldenVerzoekDto;
import nl.rivm.screenit.main.service.mamma.MammaUitwisselportaalService;
import nl.rivm.screenit.main.service.mamma.MammaUploadBeeldenService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.main.web.gebruiker.clienten.ClientPaspoortPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.MammaClientZoekenBasePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.AbstractMammaCePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.OrganisatieType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaUploadBeeldenVerzoek;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.InstellingService;
import nl.rivm.screenit.service.mamma.MammaBaseScreeningrondeService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.request.cycle.RequestCycle;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_CENTRALE_EENHEID_UPLOADVERZOEKEN },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaCeUploadBeeldenVerzoekWerklijstPage extends MammaClientZoekenBasePage
{

	@SpringBean
	private MammaUitwisselportaalService uitwisselportaalService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private MammaBaseScreeningrondeService screeningrondeService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private InstellingService instellingService;

	@SpringBean
	private MammaUploadBeeldenService uploadBeeldenService;

	private MammaCeUploadBeeldenVerzoekDataProvider dataProvider;

	private ScreenitDataTable<MammaUploadBeeldenVerzoekDto, String> table;

	private Form<MammaUploadBeeldenVerzoek> form;

	private WebMarkupContainer passport = null;

	private IModel<List<Instelling>> organisatiesModel = ModelUtil.listRModel(new ArrayList<>());

	private IModel<Instelling> organisatieModel;

	private ScreenitDropdown gedownloadDoorZiekenhuizenDropdown;

	public MammaCeUploadBeeldenVerzoekWerklijstPage()
	{
		super();

		createEmptyPassportContainer();
		createEmptyContentContainer();
		createWerklijst();
	}

	@Override
	protected void updateContent()
	{
		AjaxRequestTarget target = RequestCycle.get().find(AjaxRequestTarget.class).orElse(null);
		if (clientOpt != null)
		{
			if (clientOpt.getObject().getMammaDossier().getLaatsteScreeningRonde() == null)
			{
				error(getString("geen.screeningronde"));
				return;
			}

			passport = new ClientPaspoortPanel("paspoort", clientOpt);
			passport.setOutputMarkupId(true);
			passport.setOutputMarkupPlaceholderTag(true);
			passport.setVisible(true);
			addOrReplace(passport);
			target.add(passport);

			createContentContainer(clientOpt, target);
		}
		else
		{
			passport.setVisible(false);
			createEmptyContentContainer();
			target.add(passport, form);
		}
	}

	public void createWerklijst()
	{
		List<IColumn<MammaUploadBeeldenVerzoekDto, String>> columns = new ArrayList<>();

		columns.add(new PropertyColumn<>(Model.of("Naam"), "instellingNaam"));
		columns.add(new PropertyColumn<>(Model.of("Aantal openstaand"), "aantalOpenstaand"));
		columns.add(new PropertyColumn<>(Model.of("Telefoon"), "telefoon"));
		columns.add(new PropertyColumn<>(Model.of("Telefoon 2"), "telefoon2"));

		dataProvider = new MammaCeUploadBeeldenVerzoekDataProvider(ModelUtil.sModel(ScreenitSession.get().getScreeningOrganisatie()));
		table = new ScreenitDataTable<MammaUploadBeeldenVerzoekDto, String>("werklijst", columns, dataProvider, 10,
			Model.of("instelling(en)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<MammaUploadBeeldenVerzoekDto> model)
			{
				super.onClick(target, model);
				Instelling instelling = hibernateService.get(Instelling.class, model.getObject().getZiekenhuisId());
				setResponsePage(new MammaCeUploadBeeldenVerzoekInstellingWerklijstPage(ModelUtil.sModel(instelling)));
			}
		};

		addOrReplace(table);
	}

	private void createEmptyContentContainer()
	{
		form = new Form<>("form");
		form.setOutputMarkupId(true);
		form.setVisible(false);
		form.setOutputMarkupPlaceholderTag(true);
		form.add(new IndicatingAjaxSubmitLink("maakVerzoek")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				super.onSubmit(target);

				MammaUploadBeeldenVerzoek uploadBeeldenVerzoek = form.getModelObject();

				uploadBeeldenService.maakUploadVerzoek(uploadBeeldenVerzoek, clientOpt.getObject(), ScreenitSession.get().getLoggedInInstellingGebruiker());

				clientOpt = null;
				updateContent();
				dataProvider.resetList();
				target.add(table);
			}
		});

		form.setModel(new Model<>());
		addOrReplace(form);

		organisatieModel = new PropertyModel<>(form.getModel(), "ziekenhuis");

		gedownloadDoorZiekenhuizenDropdown = (ScreenitDropdown) new ScreenitDropdown<>("gedownloadDoorZiekenhuizen", organisatieModel,
			organisatiesModel, new ChoiceRenderer<>("naam")).setRequired(true);
		gedownloadDoorZiekenhuizenDropdown.setOutputMarkupId(true);
		gedownloadDoorZiekenhuizenDropdown.setOutputMarkupPlaceholderTag(true);
		form.addOrReplace(gedownloadDoorZiekenhuizenDropdown);
	}

	private void createEmptyPassportContainer()
	{
		passport = new EmptyPanel("paspoort");
		passport.setOutputMarkupId(true);
		passport.setVisible(false);
		passport.setOutputMarkupPlaceholderTag(true);
		add(passport);
	}

	private void createContentContainer(IModel<Client> clientOpt, AjaxRequestTarget target)
	{
		List<Instelling> organisaties = instellingService
			.getInstellingByOrganisatieTypes(Arrays.asList(OrganisatieType.MAMMAPOLI, OrganisatieType.RADIOLOGIEAFDELING, OrganisatieType.ZORGINSTELLING));
		organisatiesModel.setObject(organisaties);

		form.setVisible(true);
		MammaUploadBeeldenVerzoek uploadBeeldenVerzoek = new MammaUploadBeeldenVerzoek();
		form.setModelObject(uploadBeeldenVerzoek);
		organisatieModel.setObject(uitwisselportaalService.getLaatstGedownloadDoorInstelling(clientOpt.getObject().getMammaDossier()));
		target.add(form);
	}

	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		return AbstractMammaCePage.getContextMenuItemsList();
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(organisatiesModel);
		ModelUtil.nullSafeDetach(organisatieModel);
	}
}
