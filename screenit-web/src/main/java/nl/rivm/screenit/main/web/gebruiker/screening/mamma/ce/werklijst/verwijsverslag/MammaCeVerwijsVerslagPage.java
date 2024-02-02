package nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.werklijst.verwijsverslag;

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

import java.io.File;

import nl.rivm.screenit.main.service.mamma.MammaBeoordelingService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.modal.IDialog;
import nl.rivm.screenit.main.web.gebruiker.algemeen.documenttemplatetesten.PdfViewerPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.huisarts.MammaHuisartsBeheerPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.huisarts.MammaHuisartsZoekenPopupPanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.inzien.ClientPaspoortHorizontaal;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.AbstractMammaCePage;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.ce.popups.MammaCeVerslagAfkeurenDialog;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.panel.MammaBeoordelingPdfTonenPanel;
import nl.rivm.screenit.main.web.gebruiker.screening.mamma.panel.MammaNevenbevindingViewerPanel;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaBeoordelingStatus;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.mamma.MammaBaseBeoordelingService;
import nl.rivm.screenit.service.mamma.MammaHuisartsService;
import nl.topicuszorg.wicket.hibernate.cglib.ModelProxyHelper;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.Component;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.markup.html.form.AjaxCheckBox;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaCeVerwijsVerslagPage extends AbstractMammaCePage
{

	private final IModel<MammaBeoordeling> beoordelingModel;

	private final IModel<Boolean> directPrinten = Model.of(false);

	private final BootstrapDialog dialog;

	private final WebMarkupContainer huisartsInfoContainer;

	private final WebMarkupContainer altHuisartsInfoContainer;

	private final IModel<MammaScreeningRonde> rondeVoorAlternatieveHuisartsModel = ModelUtil.cModel(new MammaScreeningRonde());

	@SpringBean
	private MammaBeoordelingService beoordelingService;

	@SpringBean
	private MammaBaseBeoordelingService baseBeoordelingService;

	@SpringBean
	private MammaHuisartsService huisartsService;

	@SpringBean
	private ICurrentDateSupplier currentDateSupplier;

	private WebMarkupContainer verslagActiesContainer;

	public MammaCeVerwijsVerslagPage(IModel<MammaBeoordeling> beoordelingModel)
	{
		this.beoordelingModel = beoordelingModel;

		dialog = new BootstrapDialog("dialog")
		{
			@Override
			public boolean fade()
			{
				return false;
			}
		};
		add(dialog);

		maakNevenBevindingenGedeelte();

		maakPdfGedeelte();

		huisartsInfoContainer = maakHuisartsBeheerContainer();

		altHuisartsInfoContainer = maakAlternatieveHuisartsBeheerContainer();

		maakPaspoort();

		maakVervolgActiesContainer();
	}

	private void maakNevenBevindingenGedeelte()
	{
		MammaNevenbevindingViewerPanel nevenbevindingenPanel = new MammaNevenbevindingViewerPanel("nevenbevindingen", beoordelingModel);
		add(nevenbevindingenPanel);
	}

	private WebMarkupContainer maakAlternatieveHuisartsBeheerContainer()
	{
		WebMarkupContainer altHuisartsInfoContainer = new WebMarkupContainer("altHuisartsInfoContainer");
		altHuisartsInfoContainer.setOutputMarkupId(true);
		altHuisartsInfoContainer.setOutputMarkupPlaceholderTag(true);
		add(altHuisartsInfoContainer);
		altHuisartsInfoContainer.setVisible(false);
		altHuisartsInfoContainer.add(new EmptyPanel("huisartsInfo"));
		return altHuisartsInfoContainer;
	}

	private WebMarkupContainer maakHuisartsBeheerContainer()
	{
		WebMarkupContainer huisartsInfoContainer = new WebMarkupContainer("huisartsInfoContainer");
		huisartsInfoContainer.setOutputMarkupId(true);
		add(huisartsInfoContainer);
		huisartsInfoContainer.add(maakHuisartsBeheer());
		return huisartsInfoContainer;
	}

	private void maakVervolgActiesContainer()
	{
		verslagActiesContainer = new WebMarkupContainer("verslagActies");
		verslagActiesContainer.setOutputMarkupId(true);
		maakBeoordelingDoorzettenNevenbevindingenButton(verslagActiesContainer);
		maakUitslagVerzendenButton(verslagActiesContainer);
		maakVerslagAfkeurenButton(verslagActiesContainer);
		maakVerslagLaterGoedkeurenButton(verslagActiesContainer);
		maakAlternatiefHaButton(verslagActiesContainer);

		verslagActiesContainer.add(new AjaxCheckBox("directPrinten", directPrinten)
		{
			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
			}
		}.setVisible(!MammaBeoordelingStatus.GUNSTIG_MET_NEVENBEVINDING.equals(beoordelingModel.getObject().getStatus())));

		verslagActiesContainer.setVisible(ScreenitSession.get().checkPermission(Recht.GEBRUIKER_CENTRALE_EENHEID_VERWIJSVERSLAGEN_CONTROLLEREN, Actie.AANPASSEN));
		add(verslagActiesContainer);
	}

	private void maakAlternatiefHaButton(WebMarkupContainer container)
	{
		Component kiesAlternativeHa;
		MammaScreeningRonde screeningRonde = baseBeoordelingService.getScreeningRonde(beoordelingModel.getObject());
		if (screeningRonde.getGeenHuisartsOptie() != null)
		{
			kiesAlternativeHa = new EmptyPanel("kiesAlternativeHa").setVisible(false).setOutputMarkupPlaceholderTag(true);
		}
		else
		{
			kiesAlternativeHa = new IndicatingAjaxLink<Void>("kiesAlternativeHa")
			{
				@Override
				public void onClick(AjaxRequestTarget target)
				{
					dialog.openWith(target, new AlternatieveHuisartsPopupPanel());
				}
			};
		}
		container.addOrReplace(kiesAlternativeHa);
	}

	private void maakVerslagLaterGoedkeurenButton(WebMarkupContainer container)
	{
		IndicatingAjaxLink<Void> laterGoedkeurenButton = new IndicatingAjaxLink<Void>("laterGoedkeuren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				beoordelingService.verslagLaterGoedkeurenDoorCE(ModelProxyHelper.deproxy(beoordelingModel.getObject()), ScreenitSession.get().getLoggedInInstellingGebruiker());
				ScreenitSession.get().info(getString("verslag.goedkeuring.opgeschort"));
				setResponsePage(MammaCeVerwijsVerslagenWerklijstPage.class);
			}
		};
		laterGoedkeurenButton.setVisible(!MammaBeoordelingStatus.GUNSTIG_MET_NEVENBEVINDING.equals(beoordelingModel.getObject().getStatus()));
		container.add(laterGoedkeurenButton);
	}

	private void maakVerslagAfkeurenButton(WebMarkupContainer container)
	{
		IndicatingAjaxLink<Void> verslagAfkeurenButton = new IndicatingAjaxLink<Void>("verslagAfkeuren")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				dialog.openWith(target, new MammaCeVerslagAfkeurenDialog(IDialog.CONTENT_ID, beoordelingModel)
				{
					@Override
					public void close(AjaxRequestTarget target)
					{
						ScreenitSession.get().info(getString("verslag.afgekeurd"));
						setResponsePage(MammaCeVerwijsVerslagenWerklijstPage.class);
					}
				});
			}
		};
		verslagAfkeurenButton.setVisible(!MammaBeoordelingStatus.GUNSTIG_MET_NEVENBEVINDING.equals(beoordelingModel.getObject().getStatus()));
		container.add(verslagAfkeurenButton);
	}

	private void maakBeoordelingDoorzettenNevenbevindingenButton(WebMarkupContainer container)
	{
		container.add(new IndicatingAjaxLink<Void>("beoordelingDoorzetten")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MammaBeoordeling beoordeling = ModelProxyHelper.deproxy(beoordelingModel.getObject());
				MammaScreeningRonde screeningRonde = baseBeoordelingService.getScreeningRonde(beoordeling);
				beoordelingService.gunstigeUitslagMetNevenbevindingAfronden(beoordeling, rondeVoorAlternatieveHuisartsModel.getObject().getHuisarts(),
					ScreenitSession.get().getLoggedInInstellingGebruiker());
				if (screeningRonde.getHuisarts() != null || rondeVoorAlternatieveHuisartsModel.getObject().getHuisarts() != null)
				{
					ScreenitSession.get().info(getString("huisartsbericht.verstuurd"));
				}
				setResponsePage(new MammaCeVerwijsVerslagenWerklijstPage());
			}
		}
			.setVisible(MammaBeoordelingStatus.GUNSTIG_MET_NEVENBEVINDING.equals(beoordelingModel.getObject().getStatus())));

	}

	private void maakUitslagVerzendenButton(WebMarkupContainer container)
	{
		container.add(new IndicatingAjaxLink<Void>("uitslagVerzenden")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MammaBeoordeling beoordeling = ModelProxyHelper.deproxy(beoordelingModel.getObject());
				MammaScreeningRonde screeningRonde = baseBeoordelingService.getScreeningRonde(beoordeling);
				if (screeningRonde.getHuisarts() != null && screeningRonde.getHuisarts().isVerwijderd())
				{
					error(getString("geen.active.ha.geselecteerd"));
					return;
				}
				File file;
				try
				{
					file = beoordelingService.verslagGoedkeurenDoorCE(beoordeling, directPrinten.getObject(),
						rondeVoorAlternatieveHuisartsModel.getObject().getHuisarts(), ScreenitSession.get().getLoggedInInstellingGebruiker());
				}
				catch (IllegalStateException e)
				{
					setResponsePage(new MammaCeVerwijsVerslagenWerklijstPage());
					String errorKey = MammaBeoordelingStatus.GEANNULEERD.equals(beoordeling.getStatus()) ? "error.beoordeling.geannuleerd" : "error.beoordeling.onbekend";
					ScreenitSession.get().error(getString(errorKey));
					return;
				}
				if (file != null)
				{
					dialog.openWith(target, new PdfViewerPanel(IDialog.CONTENT_ID, file)
					{
						@Override
						protected void onClose(AjaxRequestTarget target)
						{
							super.onClose(target);
							setResponsePage(new MammaCeVerwijsVerslagenWerklijstPage());
						}
					});
				}
				else
				{
					setResponsePage(new MammaCeVerwijsVerslagenWerklijstPage());
					ScreenitSession.get().info(getString("uitslagbrief.aangemaakt"));
				}
				if (screeningRonde.getHuisarts() != null)
				{
					ScreenitSession.get().info(getString("huisartsbericht.verstuurd"));
				}
			}
		}.setVisible(!MammaBeoordelingStatus.GUNSTIG_MET_NEVENBEVINDING.equals(beoordelingModel.getObject().getStatus())));
	}

	private void maakPdfGedeelte()
	{
		WebMarkupContainer pdfContainer = new WebMarkupContainer("pdfContainer");
		if (MammaBeoordelingStatus.GUNSTIG_MET_NEVENBEVINDING.equals(beoordelingModel.getObject().getStatus()))
		{
			pdfContainer.add(new EmptyPanel("pdf"));
			pdfContainer.setVisible(false);
		}
		else
		{
			pdfContainer.add(new MammaBeoordelingPdfTonenPanel("pdf", beoordelingModel));
		}
		add(pdfContainer);
	}

	private Component maakHuisartsBeheer()
	{
		return new MammaHuisartsBeheerPanel("huisartsInfo", new CompoundPropertyModel<>(new PropertyModel<>(beoordelingModel, "onderzoek.afspraak.uitnodiging.screeningRonde")),
			dialog)
		{

			@Override
			protected void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts, MammaGeenHuisartsOption geenHuisartsOptie)
			{
				MammaCeVerwijsVerslagPage.this.onHuisartsGekozen(target, huisarts, geenHuisartsOptie);
			}

			@Override
			protected EnovationHuisarts getHuisartsVorigeRonde()
			{
				return huisartsService.getActieveHuisartsVanVorigeRonde(getModelObject());
			}

		}.setOutputMarkupId(true);
	}

	private void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts, MammaGeenHuisartsOption geenHuisartsOptie)
	{
		if (geenHuisartsOptie != null)
		{
			verwijderAlternatieveHuisarts(target);
		}
		MammaScreeningRonde screeningRonde = baseBeoordelingService.getScreeningRonde(beoordelingModel.getObject());
		screeningRonde.setHuisarts(huisarts);
		screeningRonde.setGeenHuisartsOptie(geenHuisartsOptie);
		screeningRonde.setDatumVastleggenHuisarts(currentDateSupplier.getDate());

		huisartsInfoContainer.addOrReplace(maakHuisartsBeheer());
		target.add(huisartsInfoContainer);

		maakAlternatiefHaButton(verslagActiesContainer);
		target.add(verslagActiesContainer);

		dialog.close(target);
	}

	private void maakPaspoort()
	{
		add(new ClientPaspoortHorizontaal("paspoort",
			new CompoundPropertyModel<>(new PropertyModel<>(beoordelingModel, "onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client")), true));

	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(beoordelingModel);
		ModelUtil.nullSafeDetach(rondeVoorAlternatieveHuisartsModel);
	}

	private void verwijderAlternatieveHuisarts(AjaxRequestTarget target)
	{
		setAltHuisarts(null);
		altHuisartsInfoContainer.setVisible(false);
		target.add(altHuisartsInfoContainer);
	}

	private void setAltHuisarts(EnovationHuisarts huisarts)
	{
		rondeVoorAlternatieveHuisartsModel.getObject().setHuisarts(huisarts);
	}

	private class AlternatieveHuisartsPopupPanel extends MammaHuisartsZoekenPopupPanel
	{

		public AlternatieveHuisartsPopupPanel()
		{
			super(IDialog.CONTENT_ID, false, false);
		}

		@Override
		protected void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts, MammaGeenHuisartsOption mammaGeenHuisartsOption)
		{
			addOrReplaceAltHuisartsInfo(target, huisarts);
		}

		private void addOrReplaceAltHuisartsInfo(AjaxRequestTarget target, EnovationHuisarts huisarts)
		{
			close(target);
			setAltHuisarts(huisarts);
			altHuisartsInfoContainer.setVisible(true);
			altHuisartsInfoContainer.addOrReplace(maakAltHuisartsPanel());
			target.add(altHuisartsInfoContainer);
		}

		private Component maakAltHuisartsPanel()
		{
			return new MammaHuisartsBeheerPanel("huisartsInfo", rondeVoorAlternatieveHuisartsModel, dialog, false)
			{

				@Override
				protected void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts, MammaGeenHuisartsOption geenHuisartsOptie)
				{
					if (huisarts != null)
					{
						MammaScreeningRonde screeningRonde = baseBeoordelingService.getScreeningRonde(beoordelingModel.getObject());
						if (huisarts.equals(screeningRonde.getHuisarts()))
						{
							error("Alternatieve huisarts mag niet dezelfde zijn als de primaire huisarts.");
						}
						else
						{
							addOrReplaceAltHuisartsInfo(target, huisarts);
						}
					}
					else
					{
						close(target);
						verwijderAlternatieveHuisarts(target);
					}
				}

				@Override
				protected EnovationHuisarts getHuisartsVorigeRonde()
				{
					return null;
				}

			}.setOutputMarkupId(true);
		}

		@Override
		protected void close(AjaxRequestTarget target)
		{
			dialog.close(target);
		}

	}
}
