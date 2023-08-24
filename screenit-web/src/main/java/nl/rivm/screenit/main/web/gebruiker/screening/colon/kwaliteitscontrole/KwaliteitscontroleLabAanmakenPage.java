package nl.rivm.screenit.main.web.gebruiker.screening.colon.kwaliteitscontrole;

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

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.List;

import nl.rivm.screenit.main.service.KwaliteitscontroleLabService;
import nl.rivm.screenit.main.service.SKMLExternSchemaService;
import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.security.SecurityConstraint;
import nl.rivm.screenit.model.Instelling;
import nl.rivm.screenit.model.colon.IFobtLaboratorium;
import nl.rivm.screenit.model.colon.SKMLExternSchema;
import nl.rivm.screenit.model.colon.SKMLExterneControleBarcode;
import nl.rivm.screenit.model.colon.SKMLInterneControleBarcode;
import nl.rivm.screenit.model.colon.SKMLInterneControleSet;
import nl.rivm.screenit.model.colon.enums.IFOBTUitslagType;
import nl.rivm.screenit.model.enums.Actie;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.LogGebeurtenis;
import nl.rivm.screenit.model.enums.Recht;
import nl.rivm.screenit.service.ICurrentDateSupplier;
import nl.rivm.screenit.service.LogService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormChoiceComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.form.RadioChoice;
import org.apache.wicket.markup.html.form.TextField;
import org.apache.wicket.markup.html.link.Link;
import org.apache.wicket.model.CompoundPropertyModel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.apache.wicket.validation.validator.StringValidator;
import org.wicketstuff.shiro.ShiroConstraint;

@SecurityConstraint(
	actie = Actie.AANPASSEN,
	checkScope = true,
	constraint = ShiroConstraint.HasPermission,
	recht = Recht.GEBRUIKER_BEHEER_REGISTRATIE_KWALITEITSCONTROLE,
	bevolkingsonderzoekScopes = { Bevolkingsonderzoek.COLON })
public class KwaliteitscontroleLabAanmakenPage extends KwaliteitscontroleLabBasePage
{

	private static final long serialVersionUID = 1L;

	private IModel<BarcodeTypeModel> barcodeTypeModel;

	private WebMarkupContainer controleContainer;

	private IModel<String> barcodeModel;

	private IModel<SKMLInterneControleSet> controleType;

	private IModel<SKMLExternSchema> schema;

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private KwaliteitscontroleLabService kwaliteitService;

	@SpringBean
	private SKMLExternSchemaService schemaService;

	@SpringBean
	private ICurrentDateSupplier dateSupplier;

	@SpringBean
	private LogService logService;

	public KwaliteitscontroleLabAanmakenPage()
	{
		barcodeModel = new CompoundPropertyModel<String>(new String());
		barcodeTypeModel = new CompoundPropertyModel<BarcodeTypeModel>(new BarcodeTypeModel());
		List<IFOBTUitslagType> keuzeLijst = new ArrayList<IFOBTUitslagType>();
		keuzeLijst.add(IFOBTUitslagType.INTERN);
		keuzeLijst.add(IFOBTUitslagType.EXTERN);
		ScreenitForm<BarcodeTypeModel> selectieForm = new ScreenitForm<>("selectieForm", barcodeTypeModel);
		RadioChoice<IFOBTUitslagType> type = new RadioChoice<IFOBTUitslagType>("type", keuzeLijst, new EnumChoiceRenderer<IFOBTUitslagType>());
		type.add(new AjaxFormChoiceComponentUpdatingBehavior()
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				WebMarkupContainer nieuweCont = maakControleContainer();
				controleContainer.replaceWith(nieuweCont);
				controleContainer = nieuweCont;
				target.add(controleContainer);
			}
		});
		type.setPrefix("<label class=\"radio\">");
		type.setSuffix("</label>");
		selectieForm.add(type);
		add(selectieForm);

		ScreenitForm<Void> aanmaakForm = new ScreenitForm<>("aanmaakForm");
		TextField<String> barcode = new TextField<String>("barcode", barcodeModel);
		barcode.setRequired(true);
		barcode.add(new StringValidator(1, 255));
		aanmaakForm.add(barcode);
		controleContainer = maakControleContainer();
		aanmaakForm.add(controleContainer);
		add(aanmaakForm);

		add(new AjaxSubmitLink("opslaan", aanmaakForm)
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				Instelling organisatie = ((ScreenitSession) getSession()).getLoggedInInstellingGebruiker().getOrganisatie();
				IFobtLaboratorium laboratorium = null;

				if (organisatie instanceof IFobtLaboratorium)
				{
					laboratorium = (IFobtLaboratorium) organisatie;
				}

				if (laboratorium != null)
				{
					String barcodeString = barcodeModel.getObject();

					if (IFOBTUitslagType.INTERN == barcodeTypeModel.getObject().getType())
					{
						if (!kwaliteitService.checkOfBarcodeAlBestaat(barcodeString))
						{
							SKMLInterneControleBarcode barcode = new SKMLInterneControleBarcode();
							barcode.setType(IFOBTUitslagType.INTERN);
							barcode.setBarcode(barcodeString);
							SKMLInterneControleSet controleSet = controleType.getObject();
							barcode.setQbaseId(controleSet.getQbaseId());
							barcode.setVolgorde(controleSet.getVolgorde());
							barcode.setControleTekst(controleSet.getControleTekst());
							barcode.setDatum(dateSupplier.getDate());
							barcode.setLaboratorium(laboratorium);
							hibernateService.saveOrUpdate(barcode);
							logService.logGebeurtenis(LogGebeurtenis.INVOERGEN_INTERNE_TEST, ScreenitSession.get().getLoggedInAccount(), Bevolkingsonderzoek.COLON);
							getSession().info("Barcode is opgeslagen.");
							setResponsePage(new KwaliteitscontroleLabAanmakenPage());
						}
						else
						{
							error("Deze barcode is reeds geregistreerd.");
						}
					}
					else if (IFOBTUitslagType.EXTERN == barcodeTypeModel.getObject().getType())
					{
						if (schema != null && schema.getObject() != null)
						{
							if (!kwaliteitService.checkOfBarcodeAlBestaat(barcodeString))
							{
								SKMLExterneControleBarcode barcode = new SKMLExterneControleBarcode();
								barcode.setBarcode(barcodeString);
								barcode.setType(IFOBTUitslagType.EXTERN);
								barcode.setDatum(dateSupplier.getDate());
								barcode.setSchema(schema.getObject());
								barcode.setLaboratorium(laboratorium);
								hibernateService.saveOrUpdate(barcode);
								logService.logGebeurtenis(LogGebeurtenis.INVOERGEN_EXTERNE_TEST, ScreenitSession.get().getLoggedInAccount(), Bevolkingsonderzoek.COLON);
								getSession().info("Barcode is opgeslagen.");
								setResponsePage(new KwaliteitscontroleLabAanmakenPage());
							}
							else
							{
								error("Deze barcode is reeds geregistreerd.");
							}
						}
						else
						{
							error("Er is een schema nodig voordat er een externe barcode kan worden geregistreerd.");
						}
					}
					else
					{
						error("Selecteer eerst een controle type.");
					}
				}
				else
				{
					error("Medewerker moet voor een FIT Laboratorium werken.");
				}
			}
		});

		add(new Link<Void>("annuleren")
		{

			private static final long serialVersionUID = 1L;

			@Override
			public void onClick()
			{
				setResponsePage(KwaliteitscontroleLabAanmakenPage.class);
			}
		});
	}

	private WebMarkupContainer maakControleContainer()
	{
		WebMarkupContainer container = new WebMarkupContainer("controle");
		container.setOutputMarkupPlaceholderTag(true);
		if (barcodeTypeModel != null && barcodeTypeModel.getObject() != null && barcodeTypeModel.getObject().getType() != null)
		{
			if (IFOBTUitslagType.INTERN.equals(barcodeTypeModel.getObject().getType()))
			{
				controleType = new Model<SKMLInterneControleSet>(kwaliteitService.laagOfHoogSample(ScreenitSession.get().getInstelling()));
				container.add(new Label("controleTekst", "Controle: " + controleType.getObject().getControleTekst()));
			}
			else
			{
				SKMLExternSchema eerstVolgendeSchema = schemaService.haalEerstvolgendeSchemaOp(dateSupplier.getDate());
				if (eerstVolgendeSchema == null)
				{
					container.add(new Label("controleTekst", Model.of("Er is geen schema beschikbaar")));
				}
				else
				{
					schema = ModelUtil.cModel(eerstVolgendeSchema);
					container.add(new Label("controleTekst", Model.of(maakSchemaTekst(eerstVolgendeSchema))));
				}
			}
		}
		else
		{
			container.add(new Label("controleTekst", Model.of("")));
			container.setVisible(false);
		}
		return container;
	}

	private String maakSchemaTekst(SKMLExternSchema schema)
	{
		SimpleDateFormat formatter = new SimpleDateFormat("dd-MM-yyyy");
		StringBuilder sb = new StringBuilder();
		sb.append("Controle: ");
		sb.append(schema.getJaar());
		sb.append(" - ");
		sb.append(schema.getRonde());
		sb.append(" - ");
		sb.append(schema.getLetter());
		sb.append(" Deadline: ");
		sb.append(formatter.format(schema.getDeadline()));
		return sb.toString();
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(barcodeTypeModel);
		ModelUtil.nullSafeDetach(barcodeModel);
		ModelUtil.nullSafeDetach(controleType);
		ModelUtil.nullSafeDetach(schema);
	}
}

class BarcodeTypeModel implements Serializable
{

	private static final long serialVersionUID = 1L;

	private IFOBTUitslagType type;

	public IFOBTUitslagType getType()
	{
		return type;
	}

	public void setType(IFOBTUitslagType type)
	{
		this.type = type;
	}
}
