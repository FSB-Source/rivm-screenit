package nl.rivm.screenit.main.web.gebruiker.screening.mamma.planning.standplaats;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.util.Date;

import nl.rivm.screenit.dao.CoordinatenDao;
import nl.rivm.screenit.main.service.mamma.MammaStandplaatsService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.base.BasePage;
import nl.rivm.screenit.main.web.component.ComponentHelper;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.component.panels.UploadDocumentFormComponentPanel;
import nl.rivm.screenit.main.web.component.validator.FileValidator;
import nl.rivm.screenit.model.PostcodeCoordinaten;
import nl.rivm.screenit.model.UploadDocument;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.FileType;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.model.mamma.MammaStandplaats;
import nl.rivm.screenit.model.mamma.MammaStandplaatsLocatie;
import nl.rivm.screenit.service.mamma.MammaBaseAfspraakService;
import nl.rivm.screenit.util.AdresUtil;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxButton;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaStandplaatsEditLocatieAdresPanel extends GenericPanel<MammaStandplaatsLocatie>
{
	@SpringBean
	private MammaStandplaatsService standplaatsService;

	@SpringBean
	private MammaBaseAfspraakService baseAfspraakService;

	@SpringBean
	private CoordinatenDao coordinatenDao;

	private final IModel<MammaStandplaats> standplaatsModel;

	private final BootstrapDialog dialog;

	public MammaStandplaatsEditLocatieAdresPanel(String id, IModel<MammaStandplaatsLocatie> model, IModel<MammaStandplaats> standplaatsModel, BootstrapDialog dialog)
	{
		super(id, model);
		this.standplaatsModel = standplaatsModel;
		this.dialog = dialog;
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		boolean tijdelijkeLocatie = Boolean.TRUE.equals(getModelObject().getTijdelijk());
		String titel = getString("titel.locatie");
		if (tijdelijkeLocatie)
		{
			titel = getString("titel.tijdelijke.locatie");
		}
		Form<Void> locatieForm = new ScreenitForm<>("locatieForm");
		add(locatieForm);

		locatieForm.add(new Label("titel", titel));
		boolean magAanpassen = ScreenitSession.get().checkPermission(Recht.GEBRUIKER_SCREENING_MAMMA_PLANNING, Actie.AANPASSEN);
		ComponentHelper.addTextField(locatieForm, "straat", true, 43, !magAanpassen);
		ComponentHelper.addTextField(locatieForm, "huisnummer", true, 10, Integer.class, !magAanpassen);
		ComponentHelper.addTextField(locatieForm, "huisnummerToevoeging", false, 2, !magAanpassen);
		ComponentHelper.newPostcodeTextField(locatieForm, "postcode", true, !magAanpassen);
		ComponentHelper.addTextField(locatieForm, "plaats", true, 200, !magAanpassen);
		ComponentHelper.addTextField(locatieForm, "startDatum", false, 10, Date.class, !magAanpassen).setVisible(tijdelijkeLocatie);
		ComponentHelper.addTextField(locatieForm, "eindDatum", false, 10, Date.class, !magAanpassen).setVisible(tijdelijkeLocatie);

		ComponentHelper.addTextArea(locatieForm, "locatieBeschrijving", false, 255, !magAanpassen);
		locatieForm.add(ComponentHelper.newCheckBox("brievenApartPrinten"));
		locatieForm.add(ComponentHelper.newCheckBox("toonHuisnummerInBrieven"));

		FileValidator validator = new FileValidator(FileType.WORD_NIEUW);

		UploadDocumentFormComponentPanel uploadDocumentFormComponentPanel = new UploadDocumentFormComponentPanel("upload",
			new PropertyModel<>(getModel(), "standplaatsLocatieBijlage"), validator);
		locatieForm.add(uploadDocumentFormComponentPanel);

		MammaStandplaatsLocatie initieleLocatie = tijdelijkeLocatie ? standplaatsModel.getObject().getTijdelijkeLocatie() : standplaatsModel.getObject().getLocatie();
		final String initieelAdres = AdresUtil.getVolledigeAdresString(initieleLocatie);
		final Date initieelStartDatum = tijdelijkeLocatie ? initieleLocatie.getStartDatum() : null;
		final Date initieelEindDatum = tijdelijkeLocatie ? initieleLocatie.getEindDatum() : null;

		locatieForm.add(new IndicatingAjaxButton("opslaan")
		{

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				String waarschuwing = "";
				MammaStandplaatsLocatie locatie = MammaStandplaatsEditLocatieAdresPanel.this.getModelObject();
				if (StringUtils.isNotBlank(locatie.getLocatieBeschrijving()))
				{
					String locatieBeschrijving = locatie.getLocatieBeschrijving();
					String[] regels = locatieBeschrijving.split("\n");
					if (regels.length > 2)
					{
						error(getString("max.regels.locatiebeschrijving.overschreden"));
					}
				}
				MammaStandplaats standplaats = standplaatsModel.getObject();
				if (locatie.getTijdelijk())
				{
					if (locatie.getStartDatum() == null || locatie.getEindDatum() == null)
					{
						error(getString("tijdelijk.start.eind.datum.verplicht"));
					}
					else if (!DateUtil.compareBefore(locatie.getStartDatum(), locatie.getEindDatum()))
					{
						error(getString("tijdelijk.start.voor.eind.datum"));
					}
					else
					{
						waarschuwing = standplaatsService.controleerUitnodigingenNaVeranderingTijdelijkeLocatie(standplaats, initieelAdres, initieelStartDatum,
							initieelEindDatum);
					}
				}
				else
				{
					PostcodeCoordinaten coordinaten = coordinatenDao.getCoordinaten(locatie);
					locatie.setPostcodeCoordinaten(coordinaten);
					if (coordinaten == null)
					{
						error(getString("geen.coordinaten"));
					}

					waarschuwing = standplaatsService.controleerUitnodigingenNaVeranderingLocatie(standplaats, initieelAdres, initieelStartDatum,
						initieelEindDatum);
				}
				if (!hasErrorMessage())
				{
					UploadDocument documentFromSelectedFile = uploadDocumentFormComponentPanel.getUploadDocumentFromSelectedFile();
					if (locatie.getStandplaatsLocatieBijlage() != null && locatie.getStandplaatsLocatieBijlage().getActief() || documentFromSelectedFile != null)
					{
						locatie.setBrievenApartPrinten(true);
					}
					boolean changed = standplaatsService.saveOrUpdateStandplaatsLocatie(locatie, documentFromSelectedFile, standplaats,
						ScreenitSession.get().getLoggedInInstellingGebruiker(), initieelAdres, initieelStartDatum,
						initieelEindDatum);
					if (changed)
					{
						if (StringUtils.isNotBlank(waarschuwing))
						{
							warn(getString(waarschuwing));
						}
						else
						{
							success(getString("message.gegevensopgeslagen"));
						}
						BasePage.markeerFormulierenOpgeslagen(target);
					}
					dialog.close(target);
					onClickOpslaan(target);
				}
			}
		});

		locatieForm.add(new IndicatingAjaxLink<Void>("close")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				onClickAnnuleren(dialog, uploadDocumentFormComponentPanel, target);
			}

		});

	}

	private void onClickAnnuleren(BootstrapDialog dialog, UploadDocumentFormComponentPanel uploadDocumentFormComponentPanel, AjaxRequestTarget target)
	{
		BasePage.markeerFormulierenOpgeslagen(target);
		if (uploadDocumentFormComponentPanel.isVerwijderd())
		{
			UploadDocument bijlage = getModelObject().getStandplaatsLocatieBijlage();
			if (bijlage != null && bijlage.getId() != null && Boolean.FALSE.equals(bijlage.getActief()))
			{
				bijlage.setActief(true);
			}
		}
		dialog.close(target);
	}

	abstract protected void onClickOpslaan(AjaxRequestTarget target);

	@Override
	protected void detachModel()
	{
		super.detachModel();

		ModelUtil.nullSafeDetach(standplaatsModel);
	}
}
