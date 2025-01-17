package nl.rivm.screenit.main.web.gebruiker.gedeeld.brievenafdrukken;

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

import java.text.SimpleDateFormat;
import java.time.Duration;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import nl.rivm.screenit.main.service.BriefService;
import nl.rivm.screenit.main.service.cervix.CervixHuisartsService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.PollingAbstractAjaxTimerBehavior;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.component.table.ActiefPropertyColumn;
import nl.rivm.screenit.main.web.component.table.AjaxImageCellPanel;
import nl.rivm.screenit.main.web.component.table.ScreenitDataTable;
import nl.rivm.screenit.main.web.component.table.booleanfilter.BooleanFilterPropertyColumn;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerBasePage;
import nl.rivm.screenit.main.web.gebruiker.base.GebruikerMenuItem;
import nl.rivm.screenit.model.Gebruiker_;
import nl.rivm.screenit.model.Instelling_;
import nl.rivm.screenit.model.MergedBrieven;
import nl.rivm.screenit.model.MergedBrievenFilter;
import nl.rivm.screenit.model.MergedBrieven_;
import nl.rivm.screenit.model.ScreeningOrganisatie;
import nl.rivm.screenit.model.UploadDocument_;
import nl.rivm.screenit.model.algemeen.AlgemeneMergedBrieven;
import nl.rivm.screenit.model.algemeen.BezwaarMergedBrieven;
import nl.rivm.screenit.model.cervix.CervixMergedBrieven;
import nl.rivm.screenit.model.cervix.CervixRegioMergedBrieven;
import nl.rivm.screenit.model.colon.ColonMergedBrieven;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.enums.ToegangLevel;
import nl.rivm.screenit.model.mamma.MammaMergedBrieven;
import nl.rivm.screenit.model.project.ProjectMergedBrieven;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.object.helper.HibernateHelper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.AjaxLink;
import org.apache.wicket.extensions.markup.html.repeater.data.grid.ICellPopulator;
import org.apache.wicket.extensions.markup.html.repeater.data.sort.SortOrder;
import org.apache.wicket.extensions.markup.html.repeater.data.table.AbstractColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.IColumn;
import org.apache.wicket.extensions.markup.html.repeater.data.table.PropertyColumn;
import org.apache.wicket.extensions.markup.html.repeater.util.SortableDataProvider;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.repeater.Item;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.LoadableDetachableModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.wicketstuff.datetime.markup.html.basic.DateLabel;

import static nl.rivm.screenit.main.util.WicketSpringDataUtil.toSpringSort;
import static nl.rivm.screenit.util.StringUtil.propertyChain;

public abstract class AfdrukkenDocumentenBasePage<MB extends MergedBrieven<?>> extends GebruikerBasePage
{
	private final class MergedBrievenDataProvider extends SortableDataProvider<MB, String>
	{
		public MergedBrievenDataProvider()
		{
			setSort(MergedBrieven_.BRIEF_TYPE, SortOrder.ASCENDING);
		}

		@Override
		public Iterator<MB> iterator(long first, long count)
		{

			return briefService
				.getMergedBrieven(ModelUtil.nullSafeGet(screeningOrganisatie), filterModel.getObject(), first, count, toSpringSort(getSort())).iterator();
		}

		@Override
		public long size()
		{
			return briefService.countMergedBrieven(ModelUtil.nullSafeGet(screeningOrganisatie), filterModel.getObject());
		}

		@Override
		public IModel<MB> model(MB object)
		{
			return ModelUtil.sModel(object);
		}
	}

	private static final long serialVersionUID = 1L;

	@SpringBean
	private BriefService briefService;

	@SpringBean
	private LogService logService;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private CervixHuisartsService cervixHuisartsService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private IModel<MergedBrievenFilter<MB>> filterModel = Model.of(new MergedBrievenFilter<>());

	private IModel<ScreeningOrganisatie> screeningOrganisatie;

	private PollingAbstractAjaxTimerBehavior timer;

	private List<Component> postfixes = new ArrayList<>();

	@SuppressWarnings("rawtypes")
	public AfdrukkenDocumentenBasePage(Class<MB> mergedBrievenClass)
	{
		filterModel.getObject().setMergedBrievenClass(mergedBrievenClass);
		filterModel.getObject().setExctMatch(true);

		filterModel.getObject().setGeprint(false);
		filterModel.getObject().setControle(null);

		final WebMarkupContainer documentContainer = new WebMarkupContainer("documentContainer");
		documentContainer.setOutputMarkupId(true);
		add(documentContainer);

		timer = new PollingAbstractAjaxTimerBehavior(Duration.of(2, ChronoUnit.SECONDS))
		{
			@Override
			protected void onTimer(AjaxRequestTarget target)
			{
				super.onTimer(target);
				for (Component comp : postfixes)
				{
					target.add(comp);
				}
			}
		};
		add(timer);

		final BootstrapDialog printDialog = new BootstrapDialog("printDialog")
		{
			@Override
			public void onClose(AjaxRequestTarget target)
			{
				super.onClose(target);
				target.add(documentContainer);
			}
		};
		add(printDialog);

		ScreeningOrganisatie screeningOrganisatieToSet = null;
		ToegangLevel toegangLevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, getPaginaRecht(mergedBrievenClass));
		if (toegangLevel != ToegangLevel.LANDELIJK)
		{
			screeningOrganisatieToSet = ScreenitSession.get().getScreeningOrganisatie();
		}
		this.screeningOrganisatie = ModelUtil.sModel(screeningOrganisatieToSet);

		List<IColumn<MB, String>> columns = new ArrayList<>();
		columns.add(
			new PropertyColumn<>(Model.of("Naam document"), propertyChain(MergedBrieven_.MERGED_BRIEVEN, UploadDocument_.NAAM),
				propertyChain(MergedBrieven_.MERGED_BRIEVEN, UploadDocument_.NAAM)));
		columns.add(new PropertyColumn<>(Model.of("Type"), propertyChain(MergedBrieven_.BRIEF_TYPE), propertyChain(MergedBrieven_.BRIEF_TYPE, "weergaveNaam")));
		if (!AlgemeneMergedBrieven.class.equals(mergedBrievenClass))
		{
			columns.add(
				new PropertyColumn<>(Model.of("Screeningsorganisatie"), propertyChain(MergedBrieven_.SCREENING_ORGANISATIE, Instelling_.NAAM),
					propertyChain(MergedBrieven_.SCREENING_ORGANISATIE, Instelling_.NAAM)));
		}
		columns.add(new PropertyColumn<>(Model.of("Datum"), propertyChain(MergedBrieven_.CREATIE_DATUM), propertyChain(MergedBrieven_.CREATIE_DATUM)));
		columns.add(new AbstractColumn<>(Model.of("Downloaden"))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MB>> cellItem, String componentId, IModel<MB> rowModel)
			{
				cellItem.add(new AjaxImageCellPanel<>(componentId, rowModel, "icon-mimetype-pdf")
				{
					@Override
					protected void onClick(AjaxRequestTarget target)
					{
						printDialog.openWith(target, new MergedBrievenPrintPanel<MB>(IDialog.CONTENT_ID, getModel()));
						MergedBrieven mergedbrieven = getModelObject();
						if (mergedbrieven.getGeprint().equals(Boolean.FALSE))
						{
							mergedbrieven.setGeprint(true);
							mergedbrieven.setPrintDatum(currentDateSupplier.getDate());
							mergedbrieven.setAfgedruktDoor(ScreenitSession.get().getLoggedInInstellingGebruiker().getMedewerker());
							if (HibernateHelper.deproxy(mergedbrieven) instanceof CervixRegioMergedBrieven)
							{
								cervixHuisartsService.updateLabformulierAanvraag((CervixRegioMergedBrieven) mergedbrieven);
							}
						}
						hibernateService.saveOrUpdate(getModelObject());
						target.add(documentContainer);

						logAction(LogGebeurtenis.BRIEF_AFGEDRUKT, (MB) getDefaultModelObject());
					}
				});
			}
		});

		columns.add(new ActiefPropertyColumn<>(Model.of("Afgedrukt"), propertyChain(MergedBrieven_.PRINT_DATUM), documentContainer, filterModel)
		{
			@Override
			public void populateItem(Item<ICellPopulator<MB>> item, String componentId, IModel<MB> rowModel)
			{
				item.add(DateLabel.forDatePattern(componentId, new PropertyModel<>(rowModel, "printDatum"), "dd-MM-yyyy"));
			}
		});

		filterModel.getObject().setControle(null);
		columns.add(new BooleanFilterPropertyColumn<>(Model.of("Gecontroleerd"), propertyChain(MergedBrieven_.CONTROLE), filterModel, documentContainer,
			propertyChain(MergedBrieven_.CONTROLEER_DATUM))
		{
			@Override
			public void populateItem(Item<ICellPopulator<MB>> item, String componentId, IModel<MB> rowModel)
			{
				MergedBrieven mBrieven = (MergedBrieven) HibernateHelper.deproxy(rowModel.getObject());
				if (mBrieven.getControle())
				{
					SimpleDateFormat format = new SimpleDateFormat("dd-MM-yyyy");
					String controleerValue = format.format(mBrieven.getControleerDatum());
					controleerValue += " (aantal: " + mBrieven.getAantalBrieven() + ")";
					item.add(new Label(componentId, Model.of(controleerValue)));
				}
				else
				{
					item.add(new AjaxImageCellPanel<>(componentId, rowModel, "icon-eye-open")
					{
						@Override
						protected void onClick(AjaxRequestTarget target)
						{
							timer.stop(target);
							timer.setNotPolling(target);
							printDialog.openWith(target, new AfdrukkenControlePanel<MB>(IDialog.CONTENT_ID, getModel())
							{
								@Override
								protected void close(AjaxRequestTarget target)
								{
									printDialog.close(target);
									timer.restartTimer(target);
									target.add(documentContainer);
								}
							});
							target.add(documentContainer);
						}
					});
				}
			}
		});
		columns.add(new PropertyColumn<>(Model.of("Gebruiker"), propertyChain(MergedBrieven_.AFGEDRUKT_DOOR, Gebruiker_.ACHTERNAAM),
			propertyChain(MergedBrieven_.AFGEDRUKT_DOOR, "naamVolledig")));

		ScreenitDataTable<MB, String> brieven = new ScreenitDataTable<>("brieven", columns, new MergedBrievenDataProvider(), new Model<>(""));

		documentContainer.add(brieven);

		AjaxLink<Void> afdrukkenBtn = new AjaxLink<>("afdrukken")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				filterModel.getObject().setGeprint(false);
				filterModel.getObject().setControle(null);
				target.add(documentContainer);
			}
		};
		add(afdrukkenBtn);

		AjaxLink<Void> controlerenBtn = new AjaxLink<>("controleren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				filterModel.getObject().setGeprint(true);
				filterModel.getObject().setControle(false);
				target.add(documentContainer);
			}
		};
		add(controlerenBtn);
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(filterModel);
		ModelUtil.nullSafeDetach(screeningOrganisatie);
	}

	@SuppressWarnings("rawtypes")
	private void logAction(LogGebeurtenis gebeurtenis, MergedBrieven mergedBrieven)
	{
		String melding = "Document afgedrukt met type:";
		if (mergedBrieven.getBriefType() == null)
		{
			melding += "ProjectBrief";
		}
		else
		{
			melding += "BriefType." + mergedBrieven.getBriefType().name();
		}
		logService.logGebeurtenis(gebeurtenis, ScreenitSession.get().getLoggedInAccount(), melding,
			mergedBrieven.getBriefType() != null ? mergedBrieven.getBriefType().getOnderzoeken() : null);
	}

	@SuppressWarnings("unchecked")
	@Override
	protected List<GebruikerMenuItem> getContextMenuItems()
	{
		List<GebruikerMenuItem> contextMenuItems = new ArrayList<>();
		contextMenuItems.add(new GebruikerMenuItem("label.tab.all.afdrukkendocumentenmammaafdrukken", MammaAfdrukkenDocumentenPage.class)
		{
			@Override
			public Component getPostfix(String id)
			{
				return getAantalPostfixLabel(id, MammaMergedBrieven.class);
			}

		});

		contextMenuItems.add(new GebruikerMenuItem("label.tab.all.afdrukkendocumentencervixafdrukken", CervixAfdrukkenDocumentenPage.class)
		{
			@Override
			public Component getPostfix(String id)
			{
				return getAantalPostfixLabel(id, CervixMergedBrieven.class);
			}

		});
		contextMenuItems.add(new GebruikerMenuItem("label.tab.all.afdrukkendocumentenregioafdrukken", CervixRegioAfdrukkenDocumentenPage.class)
		{
			@Override
			public Component getPostfix(String id)
			{
				return getAantalPostfixLabel(id, CervixRegioMergedBrieven.class);
			}
		});
		contextMenuItems.add(new GebruikerMenuItem("label.tab.all.afdrukkendocumentencolonafdrukken", ColonAfdrukkenDocumentenPage.class)
		{
			@Override
			public Component getPostfix(String id)
			{
				return getAantalPostfixLabel(id, ColonMergedBrieven.class);
			}

		});

		contextMenuItems.add(new GebruikerMenuItem("label.tab.all.afdrukkendocumentenbezwaarafdrukken", BezwaarAfdrukkenDocumentenPage.class)
		{

			@Override
			public Component getPostfix(String id)
			{
				return getAantalPostfixLabel(id, BezwaarMergedBrieven.class);
			}

		});
		contextMenuItems.add(new GebruikerMenuItem("label.tab.all.afdrukkendocumentenalgemeenafdrukken", AlgemeenAfdrukkenDocumentenPage.class)
		{

			@Override
			public Component getPostfix(String id)
			{
				return getAantalPostfixLabel(id, AlgemeneMergedBrieven.class);
			}

		});
		contextMenuItems.add(new GebruikerMenuItem("label.tab.all.afdrukkendocumentenprojectafdrukken", ProjectAfdrukkenDocumentenPage.class)
		{
			@Override
			public Component getPostfix(String id)
			{
				return getAantalPostfixLabel(id, ProjectMergedBrieven.class);
			}
		});

		return contextMenuItems;
	}

	private <MBP extends MergedBrieven<?>> Component getAantalPostfixLabel(String id, Class<MBP> clazz)
	{
		Label label = new Label(id, new LoadableDetachableModel<String>()
		{
			@Override
			protected String load()
			{
				ScreeningOrganisatie screeningOrganisatieToSet = null;
				ToegangLevel toegangLevel = ScreenitSession.get().getToegangsLevel(Actie.INZIEN, getPaginaRecht(clazz));
				if (toegangLevel != ToegangLevel.LANDELIJK)
				{
					screeningOrganisatieToSet = ScreenitSession.get().getScreeningOrganisatie();
				}

				MergedBrievenFilter<MBP> mergedBrievenFilter = new MergedBrievenFilter<>();
				mergedBrievenFilter.setMergedBrievenClass(clazz);
				mergedBrievenFilter.setGeprint(false);
				mergedBrievenFilter.setExctMatch(true);
				Long aantalBrieven = briefService.countMergedBrieven(screeningOrganisatieToSet, mergedBrievenFilter);
				return "(" + aantalBrieven + ")";
			}
		});
		label.setOutputMarkupId(true);
		postfixes.add(label);
		return label;
	}

	private Recht getPaginaRecht(Class<?> clazz)
	{
		if (ProjectMergedBrieven.class.equals(clazz))
		{
			return Recht.GEBRUIKER_SCREENING_PRINTER_PROJECTBRIEVEN;
		}
		else if (AlgemeneMergedBrieven.class.equals(clazz))
		{
			return Recht.GEBRUIKER_SCREENING_PRINTER_LANDELIJK;
		}
		else
		{
			return Recht.GEBRUIKER_SCREENING_PRINTER;
		}
	}
}
