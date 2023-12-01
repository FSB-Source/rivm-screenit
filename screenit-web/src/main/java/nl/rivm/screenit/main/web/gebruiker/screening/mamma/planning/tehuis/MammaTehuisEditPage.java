package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.tehuis;

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

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicInteger;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.dao.mamma.MammaBaseTehuisClientenDao;
import nl.rivm.screenit.main.service.mamma.MammaTehuisAdresService;
import nl.rivm.screenit.main.service.mamma.MammaTehuisService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ConfirmingIndicatingAjaxLink;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.ScreenitIndicatingAjaxSubmitLink;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.main.web.component.form.PostcodeField;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.ConfirmPanel;
import nl.rivm.screenit.main.web.component.modal.DefaultConfirmCallback;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.ClientColumn;
import nl.rivm.screenit.main.web.component.table.GeboortedatumColumn;
import nl.rivm.screenit.main.web.component.table.NotClickablePropertyColumn;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.component.table.ScreenitDateTimePropertyColumn;
import nl.rivm.screenit.main.web.component.table.VerwijderPropertyColumn;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.gedeeld.MammaDoelgroepIndicatorPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.MammaPlanningBasePage;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.TijdelijkGbaAdres;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaTehuis;
import nl.rivm.screenit.model.mamma.MammaTehuisAdres;
import nl.rivm.screenit.model.mamma.enums.MammaAfspraakStatus;
import nl.rivm.screenit.service.mamma.MammaBaseStandplaatsService;
import nl.rivm.screenit.service.mamma.MammaBaseTehuisService;
import nl.rivm.screenit.service.mamma.enums.MammaTehuisSelectie;
import nl.rivm.screenit.util.AdresUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;
import nl.topicuszorg.wicket.component.modifier.AttributeToggleModifier;
import nl.topicuszorg.wicket.hibernate.SimpleHibernateModel;
import nl.topicuszorg.wicket.hibernate.markup.form.validation.UniqueFieldValidator;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;
import nl.topicuszorg.wicket.input.validator.TelefoonnummerValidator;
import nl.topicuszorg.wicket.search.column.DateTimePropertyColumn;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.Component;
import org.apache.wicket.MarkupContainer;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.markup.head.CssHeaderItem;
import org.apache.wicket.markup.head.IHeaderResponse;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.ChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.INZIEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = { Recht.GEBRUIKER_SCREENING_MAMMA_TEHUIS },
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.MAMMA })
public class MammaTehuisEditPage extends MammaPlanningBasePage
{
	private static final long serialVersionUID = 1L;

	private final boolean magTehuisAanpassen;

	private final WebMarkupContainer tehuisClientenContainer = new WebMarkupContainer("tehuisClientenContainer");

	@SpringBean
	private MammaTehuisService tehuisService;

	@SpringBean
	private MammaBaseTehuisService baseTehuisService;

	@SpringBean
	private MammaTehuisAdresService tehuisAdresService;

	@SpringBean
	private MammaBaseTehuisClientenDao baseTehuisClientenDao;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private MammaBaseStandplaatsService standplaatsService;

	private IModel<MammaStandplaats> standplaatsModel;

	private BootstrapDialog dialog;

	private IndicatingAjaxLink<Void> adresToevoegenBtn;

	private AjaxLink<Gebruiker> inActiverenBtn;

	private AjaxLink<Gebruiker> uitnodigenBtn;

	private MammaTehuisOpmerkingenPanel opmerkingenPanel;

	@Override
	public void renderHead(IHeaderResponse response)
	{
		super.renderHead(response);
		response.render(CssHeaderItem.forUrl("assets/font-awesome/css/font-awesome.min.css"));
	}

	private final WebMarkupContainer persistentContainer = new WebMarkupContainer("persistentContainer")
	{
		@Override
		protected void onConfigure()
		{
			super.onConfigure();
			setVisible(getTehuis().getId() != null);
		}
	};

	public MammaTehuisEditPage(IModel<MammaTehuis> model)
	{
		setDefaultModel(model);

		dialog = new BootstrapDialog("dialog");
		add(dialog);

		standplaatsModel = model.getObject().getStandplaats() != null ? ModelUtil.sModel(model.getObject().getStandplaats()) : new SimpleHibernateModel<>();

		magTehuisAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_TEHUIS, Actie.AANPASSEN) && ingelogdNamensRegio;

		Form<MammaTehuis> tehuisWijzigenForm = new ScreenitForm<>("tehuisWijzigenForm", model);
		createOrReplaceNaamComponent(tehuisWijzigenForm, null);

		tehuisWijzigenForm.add(maakStandplaatsenDropdown(model.getObject().getStandplaats()));

		ComponentHelper.addTextField(tehuisWijzigenForm, "contactpersoon", true, 255, !magTehuisAanpassen);

		ComponentHelper.addTextField(tehuisWijzigenForm, "aanschrijfAdres.straat", false, 43, !magTehuisAanpassen);
		ComponentHelper.addTextField(tehuisWijzigenForm, "aanschrijfAdres.huisnummer", true, 10, Integer.class, !magTehuisAanpassen);
		ComponentHelper.addTextField(tehuisWijzigenForm, "aanschrijfAdres.huisnummerToevoeging", false, 26, !magTehuisAanpassen);
		ComponentHelper.addTextField(tehuisWijzigenForm, "aanschrijfAdres.huisletter", false, 1, !magTehuisAanpassen);
		ComponentHelper.newPostcodeTextField(tehuisWijzigenForm, "aanschrijfAdres.postcode", true, !magTehuisAanpassen);
		ComponentHelper.addTextField(tehuisWijzigenForm, "aanschrijfAdres.plaats", true, 200, !magTehuisAanpassen);
		ComponentHelper.addTextField(tehuisWijzigenForm, "telefoonnummer", false, 255, !magTehuisAanpassen).add(TelefoonnummerValidator.alle());

		opmerkingenPanel = new MammaTehuisOpmerkingenPanel("opmerkingen", model)
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				opmerkingenPanel.setVisible(model.getObject().getId() != null);
			}
		};
		opmerkingenPanel.setOutputMarkupId(true);

		tehuisWijzigenForm.add(opmerkingenPanel);

		tehuisWijzigenForm.add(new ScreenitIndicatingAjaxSubmitLink("opslaan", tehuisWijzigenForm)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				MammaTehuis tehuis = (MammaTehuis) getForm().getModelObject();
				MammaStandplaats origineleStandplaats = tehuis.getStandplaats();
				MammaStandplaats nieuweStandplaats = standplaatsModel.getObject();
				tehuis.setStandplaats(nieuweStandplaats);
				nieuweStandplaats.getTehuizen().add(tehuis);

				boolean succes = baseTehuisService.saveOrUpdateTehuis(tehuis, origineleStandplaats, ScreenitSession.get().getLoggedInInstellingGebruiker());
				if (succes)
				{
					success(getString("message.gegevensopgeslagen"));
					createOrReplaceNaamComponent(tehuisWijzigenForm, target);
					BasePage.markeerFormulierenOpgeslagen(target);
					target.add(persistentContainer);
					target.add(opmerkingenPanel);
					target.add(inActiverenBtn);
					target.add(uitnodigenBtn);
				}
			}
		}.setVisible(magTehuisAanpassen));

		persistentContainer.setOutputMarkupId(true);
		persistentContainer.setOutputMarkupPlaceholderTag(true);
		add(persistentContainer);

		createAdresToevoegenForm();
		createClientZoekenForm();

		addInActiverenButton(tehuisWijzigenForm);
		addUitnodigenButton(tehuisWijzigenForm);

		add(tehuisWijzigenForm);

		add(new IndicatingAjaxLink<Void>("terug")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				setResponsePage(MammaTehuisZoekenPage.class);
			}
		});

	}

	private void createAdresToevoegenForm()
	{
		WebMarkupContainer tehuisAdressenContainer = new WebMarkupContainer("tehuisAdressenContainer");
		tehuisAdressenContainer.setOutputMarkupId(true);
		persistentContainer.add(tehuisAdressenContainer);

		List<IColumn<MammaTehuisAdres, String>> columns = new ArrayList<>();
		columns.add(new PropertyColumn<>(Model.of("Postcode"), "postcode", "postcode"));
		columns.add(new PropertyColumn<>(Model.of("Huisnummer"), "huisnummer", "huisnummer"));
		columns.add(new PropertyColumn<>(Model.of("Huisnummertoevoeging"), "huisnummerToevoeging", "huisnummerToevoeging"));
		columns.add(new PropertyColumn<>(Model.of("Huisletter"), "huisletter", "huisletter"));
		columns.add(new PropertyColumn<>(Model.of("Locatie van tehuis"), "locatieVanTehuis", "locatieVanTehuis")
		{
			@Override
			public IModel<String> getDataModel(IModel<MammaTehuisAdres> rowModel)
			{
				return Model.of(rowModel.getObject().getLocatieVanTehuis() ? "Ja" : "Nee");
			}
		});

		if (magTehuisAanpassen)
		{
			columns.add(new VerwijderPropertyColumn<MammaTehuisAdres, String>(Model.of("Verwijderen"), StringUtils.EMPTY)
			{

				@Override
				public void onClickDeleteAction(AjaxRequestTarget target, IModel<MammaTehuisAdres> adresModel)
				{
					MammaTehuisAdres adres = adresModel.getObject();
					if (baseTehuisClientenDao.countClienten(adres.getTehuis(), MammaTehuisSelectie.GEKOPPELD, adres) > 0)
					{
						error(getString("error.adres.kan.niet.verwijderd.worden"));
					}
					else
					{
						getTehuis().getAdressen().remove(adres);
						tehuisAdresService.adresVerwijderen(adres, ScreenitSession.get().getLoggedInInstellingGebruiker());
						target.add(tehuisAdressenContainer);
						target.add(tehuisClientenContainer);
					}
				}
			});
		}

		ScreenitDataTable tehuisAdressenTabel = new ScreenitDataTable<>("tehuisAdressenTabel", columns, new MammaTehuisAdressenDataProvider("plaats", getTehuisModel()), 10,
			Model.of("adres(sen)"))
		{
			@Override
			protected boolean isRowClickable(IModel<MammaTehuisAdres> rowModel)
			{
				return false;
			}
		};
		tehuisAdressenContainer.add(tehuisAdressenTabel);

		adresToevoegenBtn = new IndicatingAjaxLink<>("adresToevoegenBtn")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MammaTehuisAdres tehuisAdres = new MammaTehuisAdres();
				tehuisAdres.setTehuis(getTehuis());
				tehuisAdres.setLocatieVanTehuis(true);
				openAdresDialog(target, tehuisAdressenContainer, tehuisAdres);
			}
		};
		persistentContainer.add(adresToevoegenBtn.setOutputMarkupId(true).setVisible(magTehuisAanpassen && getTehuis().getActief()));
	}

	private void openAdresDialog(AjaxRequestTarget target, WebMarkupContainer tehuisAdressenContainer, MammaTehuisAdres adres)
	{
		dialog.openWith(target,
			new MammaTehuisAdresToevoegenPanel(IDialog.CONTENT_ID, ModelUtil.ccModel(adres))
			{

				private static final long serialVersionUID = 1L;

				@Override
				protected void onClickOpslaan(AjaxRequestTarget target)
				{
					target.add(tehuisClientenContainer);
					target.add(tehuisAdressenContainer);
					dialog.close(target);
				}

			});
	}

	private void createClientZoekenForm()
	{
		MammaTehuisAdres adres = new MammaTehuisAdres();
		adres.setTehuis(getTehuis());
		IModel<MammaTehuisAdres> clientZoekenModel = ModelUtil.ccModel(adres);
		Form<MammaTehuisAdres> clientZoekenForm = new ScreenitForm<>("clientZoekenForm", clientZoekenModel);
		clientZoekenForm.add(new PostcodeField("postcode"));
		clientZoekenForm.add(new TextField<>("huisnummer"));
		clientZoekenForm.add(new TextField<>("huisnummerToevoeging"));
		clientZoekenForm.add(new TextField<>("huisletter"));

		tehuisClientenContainer.setOutputMarkupId(true);
		persistentContainer.add(tehuisClientenContainer);

		List<IColumn<Client, String>> columns = new ArrayList<>();
		columns.add(new ClientColumn<>("persoon.achternaam", ""));
		columns.add(new PropertyColumn<>(Model.of("BSN"), "persoon.bsn", "persoon.bsn"));
		columns.add(new GeboortedatumColumn<>("persoon.geboortedatum", "persoon"));
		columns.add(new PropertyColumn<>(Model.of("Adres"), "gba_adres.straat", "persoon.gbaAdres.getAdres")
		{
			@Override
			public IModel<String> getDataModel(IModel<Client> rowModel)
			{
				TijdelijkGbaAdres tijdelijkGbaAdres = rowModel.getObject().getPersoon().getTijdelijkGbaAdres();
				return Model.of(tijdelijkGbaAdres != null ? AdresUtil.getVolledigeAdresString(tijdelijkGbaAdres)
					: AdresUtil.getVolledigeAdresString(rowModel.getObject().getPersoon().getGbaAdres()));
			}
		});
		columns.add(new DateTimePropertyColumn<>(Model.of("Laatste adreswijziging"), "laatsteGbaMutatie.mutatieDatum", "laatste_gba_mutatie.mutatie_datum"));
		columns.add(new AbstractColumn<>(Model.of("Doelgroep"), "dossier.doelgroep")
		{
			@Override
			public void populateItem(Item<ICellPopulator<Client>> cellItem, String componentId, IModel<Client> rowModel)
			{
				cellItem.add(new MammaDoelgroepIndicatorPanel(componentId, rowModel.getObject().getMammaDossier(), false));
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Gekoppeld"), "mammaDossier.tehuis")
		{
			@Override
			public IModel<String> getDataModel(IModel<Client> rowModel)
			{
				MammaTehuis tehuis = rowModel.getObject().getMammaDossier().getTehuis();
				if (tehuis != null && tehuis.equals(getTehuis()))
				{
					return Model.of("Ja");
				}
				else
				{
					return Model.of("Nee");
				}
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Uit te nodigen"), "uitTeNodigen", "mammaDossier.uitTeNodigen")
		{
			@Override
			public IModel<String> getDataModel(IModel<Client> rowModel)
			{
				Boolean uitTeNodigen = rowModel.getObject().getMammaDossier().getUitTeNodigen();
				if (uitTeNodigen == null)
				{
					return Model.of("");
				}
				else if (uitTeNodigen)
				{
					return Model.of("Ja");
				}
				else
				{
					return Model.of("Nee");
				}
			}
		});
		columns.add(
			new ScreenitDateTimePropertyColumn<>(Model.of("Laatste afspraak"), "mammaDossier.laatsteScreeningRonde.laatsteUitnodiging.laatsteAfspraak.vanaf", Constants.getDateTimeFormat())
			{
				@Override
				public IModel<Object> getDataModel(IModel<Client> embeddedModel)
				{
					IModel<Object> vanafStringModel = super.getDataModel(embeddedModel);
					if (StringUtils.isNotBlank(vanafStringModel.getObject().toString()) && MammaAfspraakStatus
						.isGeannuleerd(embeddedModel.getObject().getMammaDossier().getLaatsteScreeningRonde().getLaatsteUitnodiging().getLaatsteAfspraak().getStatus()))
					{
						return new Model("");
					}
					return vanafStringModel;
				}
			});
		columns.add(new NotClickablePropertyColumn<>(Model.of(""), "")
		{
			@Override
			public void populateItem(Item<ICellPopulator<Client>> cell, String id, IModel<Client> model)
			{
				cell.add(new MammaTehuisNavigatiePanel(id, model));
			}
		});

		ScreenitDataTable<Client, String> tehuisClientenTabel = new ScreenitDataTable<>("tehuisClientenTabel", columns,
			new MammaTehuisClientenDataProvider("persoon.achternaam", clientZoekenModel), 10, Model.of("cliënt(en)"))
		{
			@Override
			public void onClick(AjaxRequestTarget target, IModel<Client> model)
			{
				MammaTehuis gekoppeldeTehuis = model.getObject().getMammaDossier().getTehuis();
				final boolean isGekoppeld = gekoppeldeTehuis != null;
				String header = "Koppelen aan tehuis";
				String content = "Weet u zeker dat u deze cliënt wilt koppelen?";

				if (isGekoppeld)
				{
					header = "Ontkoppelen van tehuis";
					content = "Weet u zeker dat u deze cliënt wilt ontkoppelen?";
				}

				if (isGekoppeld && !gekoppeldeTehuis.getId().equals(getTehuis().getId()))
				{
					error(getString("error.client.al.gekoppeld.aan.ander.tehuis"));
				}
				else if (!getTehuis().getActief() && !isGekoppeld)
				{
					error(getString("error.client.toevoegen.aan.inactief.tehuis"));
				}
				else
				{
					dialog.setContent(new ConfirmPanel(IDialog.CONTENT_ID, 
						Model.of(header), 
						Model.of(content), 
						new DefaultConfirmCallback()
						{
							@Override
							public void onYesClick(AjaxRequestTarget target)
							{
								List<String> meldingen;
								if (isGekoppeld)
								{
									meldingen = tehuisService.ontkoppelClient(getTehuis(), model.getObject());
								}
								else
								{
									meldingen = tehuisService.koppelClient(getTehuis(), model.getObject());
								}

								if (!meldingen.isEmpty())
								{
									meldingen.forEach(melding -> warn(getString(melding)));
								}

								target.add(inActiverenBtn);
								target.add(tehuisClientenContainer);
								target.add(uitnodigenBtn);
							}
						}, dialog));
					dialog.open(target);
				}
			}

			@Override
			protected boolean isRowClickable(IModel<Client> rowModel)
			{
				return super.isRowClickable(rowModel) && magTehuisAanpassen;
			}
		};

		tehuisClientenContainer.add(tehuisClientenTabel);

		IndicatingAjaxSubmitLink zoekenButton = new IndicatingAjaxSubmitLink("clientZoekenBtn", clientZoekenForm)
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				target.add(tehuisClientenContainer);
			}
		};
		clientZoekenForm.setDefaultButton(zoekenButton);
		clientZoekenForm.add(zoekenButton);

		persistentContainer.add(clientZoekenForm);
	}

	private void createOrReplaceNaamComponent(Form<MammaTehuis> tehuisWijzigenForm, AjaxRequestTarget target)
	{
		ComponentHelper.addTextField(tehuisWijzigenForm, "naam", true, 255, String.class, !magTehuisAanpassen) 
			.add(new UniqueFieldValidator<>(MammaTehuis.class, getTehuis().getId(), "naam", hibernateService));

		if (target != null)
		{
			target.add(tehuisWijzigenForm);
		}
	}

	private ScreenitDropdown<MammaStandplaats> maakStandplaatsenDropdown(MammaStandplaats huidigeStandplaats)
	{
		List<MammaStandplaats> mogelijkeStandplaatsen = getMogelijkeStandplaatsen(huidigeStandplaats);

		ScreenitDropdown<MammaStandplaats> standplaatsenDropdown = new ScreenitDropdown<>("standplaats", standplaatsModel,
			ModelUtil.listRModel(mogelijkeStandplaatsen, false), new ChoiceRenderer<>("naam"));

		standplaatsenDropdown.setRequired(true);
		standplaatsenDropdown.setEnabled(magTehuisAanpassen);

		return standplaatsenDropdown;
	}

	private List<MammaStandplaats> getMogelijkeStandplaatsen(MammaStandplaats huidigeStandplaats)
	{
		ScreeningOrganisatie regio = ScreenitSession.get().getScreeningOrganisatie();
		List<MammaStandplaats> mogelijkeStandplaatsen = standplaatsService.getActieveStandplaatsen(regio);

		if (huidigeStandplaats != null && !mogelijkeStandplaatsen.contains(huidigeStandplaats))
		{
			mogelijkeStandplaatsen.add(huidigeStandplaats);
		}

		return mogelijkeStandplaatsen;
	}

	private void addInActiverenButton(Form<MammaTehuis> tehuisWijzigenForm)
	{
		inActiverenBtn = new ConfirmingIndicatingAjaxLink<>("inActiverenBtn", dialog, "question.remove.tehuis")
		{

			@Override
			protected void onConfigure()
			{
				super.onConfigure();
				boolean magInActiveren = getTehuis().getId() != null && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_TEHUIS, Actie.VERWIJDEREN)
					&& ingelogdNamensRegio;
				boolean isEnabled = getTehuis().getDossiers().isEmpty() || !getTehuis().getActief();
				inActiverenBtn.setEnabled(isEnabled);
				inActiverenBtn.add(new AttributeToggleModifier("title", Model.of(getString("inactiveren.title")))
				{
					@Override
					public boolean isAppendValue(Component component)
					{
						return !isEnabled;
					}
				});
				inActiverenBtn.setVisible(magInActiveren);
			}

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MammaTehuis tehuis = getTehuis();
				tehuis.setActief(Boolean.FALSE.equals(tehuis.getActief()));
				if (Boolean.FALSE.equals(tehuis.getActief()))
				{
					tehuisService.deactiveerTehuis(tehuis, ScreenitSession.get().getLoggedInInstellingGebruiker());
				}
				else
				{
					baseTehuisService.saveOrUpdateTehuis(tehuis, ScreenitSession.get().getLoggedInInstellingGebruiker());
				}
				setResponsePage(MammaTehuisZoekenPage.class);
			}

			@Override
			protected boolean skipConfirmation()
			{
				return Boolean.FALSE.equals(getTehuis().getActief());
			}

		};
		inActiverenBtn.setOutputMarkupId(true);
		if (Boolean.FALSE.equals(getTehuis().getActief()))
		{
			inActiverenBtn.add(new Label("inActiverenTitle", "Activeren"));
		}
		else
		{
			inActiverenBtn.add(new Label("inActiverenTitle", "Inactiveren"));
		}

		tehuisWijzigenForm.add(inActiverenBtn);
	}

	private void addUitnodigenButton(MarkupContainer container)
	{
		uitnodigenBtn = new ConfirmingIndicatingAjaxLink<>("uitnodigen", dialog, "question.uitnodigen")
		{
			@Override
			protected void onConfigure()
			{
				super.onConfigure();

				boolean uitnodigenVisible = false;
				boolean uitnodigenEnabled = false;

				MammaTehuis tehuis = getTehuis();
				if (tehuis.getId() != null)
				{
					uitnodigenVisible = ingelogdNamensRegio && ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_TEHUIS, Actie.AANPASSEN);
					uitnodigenEnabled = baseTehuisClientenDao.countClienten(tehuis, MammaTehuisSelectie.UIT_TE_NODIGEN, null) > 0;
				}

				setVisible(uitnodigenVisible);
				setEnabled(uitnodigenEnabled);
			}

			@Override
			public void onClick(AjaxRequestTarget target)
			{
				AtomicInteger aantalClientenMetProjectBrief = new AtomicInteger(0);
				AtomicInteger aantalClientenMetBrief = new AtomicInteger(0);
				AtomicInteger aantalClientenMetSuspectBrief = new AtomicInteger(0);
				tehuisService.uitnodigen(getTehuis(), aantalClientenMetProjectBrief, aantalClientenMetBrief, aantalClientenMetSuspectBrief,
					ScreenitSession.get().getLoggedInInstellingGebruiker());

				if (aantalClientenMetBrief.get() > 0 || aantalClientenMetProjectBrief.get() > 0 || aantalClientenMetSuspectBrief.get() > 0)
				{
					dialog.openWith(target, new MammaTehuisMergedBrievenPanel(IDialog.CONTENT_ID, aantalClientenMetProjectBrief.get(),
						aantalClientenMetBrief.get() + aantalClientenMetSuspectBrief.get(), aantalClientenMetSuspectBrief.get()));
					target.add(opmerkingenPanel);
					target.add(tehuisClientenContainer);
				}
				else
				{
					error(
						"Geen open uitnodigingen gemaakt. Of alle clienten hebben al uitnodigingen voor de huidige ronde en/of van een aantal clienten is te recent een onderzoek geweest.");
				}
			}

			@Override
			protected boolean skipConfirmation()
			{
				return Boolean.FALSE.equals(getTehuis().getActief());
			}

		};
		uitnodigenBtn.setOutputMarkupId(true);

		container.add(uitnodigenBtn);
	}

	private IModel<MammaTehuis> getTehuisModel()
	{
		return (IModel<MammaTehuis>) MammaTehuisEditPage.this.getDefaultModel();
	}

	private MammaTehuis getTehuis()
	{
		return (MammaTehuis) getDefaultModelObject();
	}

	@Override
	protected Class<? extends GebruikerBasePage> getActiveContextMenuClass()
	{
		return MammaTehuisZoekenPage.class;
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(standplaatsModel);
	}
}
