package nl.rivm.screenit.main.web.gebruiker.testen.hl7bericht;

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

import java.util.Arrays;

import nl.rivm.screenit.main.service.HL7TestMessageService;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.main.web.component.dropdown.ScreenitDropdown;
import nl.rivm.screenit.model.berichten.HL7TestMessageWrapper;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.ajax.form.AjaxFormComponentUpdatingBehavior;
import org.apache.wicket.ajax.markup.html.form.AjaxSubmitLink;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.form.EnumChoiceRenderer;
import org.apache.wicket.markup.html.form.TextArea;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TestHL7BerichtPanel extends Panel
{
	@SpringBean
	private HL7TestMessageService hl7TestMessageService;

	private IModel<TestHL7BerichtTypeEnum> testBerichtTypeModel;

	private final WebMarkupContainer ifobtBerichtHandleiding = new WebMarkupContainer("ifobtBerichtHandleiding");

	private final WebMarkupContainer ormBerichtHandleiding = new WebMarkupContainer("ormBerichtHandleiding");

	@SpringBean
	private HibernateService hibernateService;

	private static final Logger LOG = LoggerFactory.getLogger(TestHL7BerichtPanel.class);

	private final static String ormTestBericht = "MSH|^~\\&|SCREENIT_MAMMA|SCREENIT|IMS|SECTRA|20180703084745||ORM^O01^ORM_O01|MESSAGE_ID|P|2.4||||||8859/1\n" +
		"PID|1||BSN^^^NLMINBIZA^NNNLD||van Doe&van&Doe^J.||19500101|||||||||||||||||||||||N\n" +
		"ORC|XO|1|||SC\n" +
		"OBR||UITNODIGING_ID||MAMMO^Mammografie|R||20180703091331||||||||||||||||||STATUS_CODE||^^^20180703084745^^^SE_CODE";

	private final static String ormTestCentraalBeschikbaarBericht = "MSH|^~\\&|SCREENIT_MAMMA|SCREENIT|IMS|SECTRA|20180703084745||ORM^O01^ORM_O01|MESSAGE_ID|P|2.4||||||8859/1\n" +
		"PID|1||BSN^^^NLMINBIZA^NNNLD||van Doe&van&Doe^J.||19500101|||||||||||||||||||||||N\n" +
		"ORC|XO|1|||SC\n" +
		"OBR||UITNODIGING_ID||MAMMO^Mammografie|R||20180703091331||||||||||||||||||CentralAvailable||^^^20180703084745^^^SE_CODE";

	private final static String ormUploadBeeldenTestBericht = "MSH|^~\\&|SCREENIT_MAMMA|SCREENIT|IMS|SECTRA|20180703084745||ORM^O01^ORM_O01|MESSAGE_ID|P|2.4||||||8859/1 \n" +
		"PID|1||BSN^^^NLMINBIZA^NNNLD||van Doe&van&Doe^J.||19700206|||||||||||||||||||||||N \n" +
		"ORC|XO|1|||SC \n" +
		"OBR||UITNODIGING_ID||ZHOND^Ziekenhuis|R||20180703091331||||||||||||||||||STATUS_CODE||^^^20180703084745^^^ZHOND";

	private final static String ormUploadBeeldenTestCentraalBeschikbaarBericht = "MSH|^~\\&|SCREENIT_MAMMA|SCREENIT|IMS|SECTRA|20180703084745||ORM^O01^ORM_O01|MESSAGE_ID|P|2.4||||||8859/1\n"
		+
		"PID|1||BSN^^^NLMINBIZA^NNNLD||van Doe&van&Doe^J.||19700206|||||||||||||||||||||||N\n" +
		"ORC|XO|1|||SC\n" +
		"OBR||UITNODIGING_ID||ZHOND^Ziekenhuis|R||20180703091331||||||||||||||||||CentralAvailable||^^^20180703084745^^^ZHOND";

	private final static String ormIlmUploadBeeldenTestBericht = "MSH|^~\\&|SCREENIT_MAMMA|SCREENIT|IMS|SECTRA|20200226165332||ORM^O01^ORM_O01|MESSAGE_ID|P|2.4||||||8859/1 \n" +
		"PID|1||BSN^^^NLMINBIZA^NNNLD||de Doe?-460160771&de&Doe?-460160771^J.||19700211|F||||||||||||||||||||||N \n" +
		"ORC|DC|1|||IP \n" +
		"OBR||UITNODIGING_ID||ZHOND^Ziekenhuis|R||||||||||||||||||||Deleted||^^^^^^ZHOND";

	private final static String adtTestBericht = "MSH|^~\\&|SCREENIT_MAMMA|SCREENIT|IMS|SECTRA|20180711144339||ADT^A08^ADT_A01|13101|P|2.4||||||8859/1\n" +
		"PID|1||888800000^^^NLMINBIZA^NNNLD||van Doe&van&Doe^J.||19500101|||||||||||||||||||||||N";

	private final static String fitTestBericht = "MSH|^~\\&|SCREENIT_TEST|LAB_ID|SCREENIT|TOPICUS|20170213133912||OUL^R22^OUL_R22|MESSAGE_ID|P|2.5.1||||||UNICODE UTF-8|||LAB-29^IHE\n"
		+
		"SPM|1|||119339001^Stool specimen (specimen)^SNOMED\n" +
		"SAC|||FIT_BARCODE\n" +
		"OBR||||58453-2^Hemoglobine.gastrointestinaal^LN\n" +
		"OBX|1|NM|58453-2^Hemoglobine.gastrointestinaal^LN||UITSLAG||||||F|||||||BioM690069^SYSMEX|ANALYSE_DATUM\n" +
		"SPM|2|||119339001^Stool specimen (specimen)^SNM\n" +
		"SAC|||FIT_BARCODE\n" +
		"OBR||||58453-2^Hemoglobine.gastrointestinaal^LN\n" +
		"OBX|1|NM|58453-2^Hemoglobine.gastrointestinaal^LN||UITSLAG||||||F|||||||BioM690069^SYSMEX|ANALYSE_DATUM\n" +
		"SPM|3|||119339001^Stool specimen (specimen)^SNOMED\n" +
		"SAC|||FIT_BARCODE\n" +
		"OBR||||58453-2^Hemoglobine.gastrointestinaal^LN\n" +
		"OBX|1|NM|58453-2^Hemoglobine.gastrointestinaal^LN||UITSLAG||||||F|||||||BioM690069^SYSMEX|ANALYSE_DATUM";

	private final static String ilmBerichtVerwijderdTestBericht = "MSH|^~\\&|SCREENIT_MAMMA|SCREENIT|IMS|SECTRA|20181003162245||ORM^O01^ORM_O01|MESSAGE_ID|P|2.4||||||8859/1|\n" +
		"PID|1||BSN^^^NLMINBIZA^NNNLD||de Doe&van&Doe?^J.||19681003|F||||||||||||||||||||||N|\n" +
		"ORC|OD|1|||CA|\n" +
		"OBR||UITNODIGING_ID||MAMMO^Mammografie|R||||||||||||||||||||Deleted||^^^20181102130000^^^SE-UUID-SO0|";

	private IModel<String> hl7BerichtTekst = Model
		.of(fitTestBericht);

	public TestHL7BerichtPanel(String id)
	{
		super(id);

		ifobtBerichtHandleiding.setOutputMarkupId(true);
		ifobtBerichtHandleiding.setOutputMarkupPlaceholderTag(true);
		add(ifobtBerichtHandleiding);

		ormBerichtHandleiding.setOutputMarkupId(true);
		ormBerichtHandleiding.setOutputMarkupPlaceholderTag(true);
		ormBerichtHandleiding.setVisible(false);
		add(ormBerichtHandleiding);

		ScreenitForm form = new ScreenitForm<>("form");
		form.setOutputMarkupId(true);
		add(form);

		ScreenitDropdown<TestHL7BerichtTypeEnum> berichtTypeDropdown = new ScreenitDropdown<>("berichtType", Arrays.asList(TestHL7BerichtTypeEnum.values()),
			new EnumChoiceRenderer<>());
		testBerichtTypeModel = Model.of(TestHL7BerichtTypeEnum.DK_IFOBT);
		berichtTypeDropdown.setNullValid(false);
		berichtTypeDropdown.setDefaultModel(testBerichtTypeModel);
		berichtTypeDropdown.setRequired(true);
		form.add(berichtTypeDropdown);
		berichtTypeDropdown.add(new AjaxFormComponentUpdatingBehavior("change")
		{

			private static final long serialVersionUID = 1L;

			@Override
			protected void onUpdate(AjaxRequestTarget target)
			{
				if (testBerichtTypeModel.getObject().equals(TestHL7BerichtTypeEnum.DK_IFOBT))
				{
					ormBerichtHandleiding.setVisible(Boolean.FALSE);
					ifobtBerichtHandleiding.setVisible(Boolean.TRUE);
					hl7BerichtTekst.setObject(fitTestBericht);
				}
				else
				{
					ifobtBerichtHandleiding.setVisible(Boolean.FALSE);
					ormBerichtHandleiding.setVisible(Boolean.TRUE);
					switch (testBerichtTypeModel.getObject())
					{
					case MAMMA_ADT:
						hl7BerichtTekst.setObject(adtTestBericht);
						break;
					case MAMMA_ORM:
						hl7BerichtTekst.setObject(ormTestBericht);
						break;
					case MAMMA_ORM_VANUIT_IMS:
						hl7BerichtTekst.setObject(ormTestCentraalBeschikbaarBericht);
						break;
					case MAMMA_ORM_UPLOAD_BEELDEN:
						hl7BerichtTekst.setObject(ormUploadBeeldenTestBericht);
						break;
					case MAMMA_ORM_UPLOAD_BEELDEN_VANUIT_IMS:
						hl7BerichtTekst.setObject(ormUploadBeeldenTestCentraalBeschikbaarBericht);
						break;
					case MAMMA_ILM_UPLOAD_BEELDEN_VANUIT_IMS:
						hl7BerichtTekst.setObject(ormIlmUploadBeeldenTestBericht);
						break;
					case MAMMA_ILM_VANUIT_IMS:
						hl7BerichtTekst.setObject(ilmBerichtVerwijderdTestBericht);
						break;
					default:
						break;
					}
				}
				target.add(ifobtBerichtHandleiding);
				target.add(ormBerichtHandleiding);
				target.add(form);
			}
		});

		TextArea textArea = new TextArea<>("hl7BerichtTekst", hl7BerichtTekst);
		textArea.setOutputMarkupId(true);
		form.add(textArea);
		form.add(new AjaxSubmitLink("verstuurHl7Bericht")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				try
				{

					HL7TestMessageWrapper responseMessageWrapper = null;
					switch (testBerichtTypeModel.getObject())
					{
					case DK_IFOBT:
						responseMessageWrapper = hl7TestMessageService.verstuurIFobtTestBericht(hl7BerichtTekst.getObject());
						break;
					case MAMMA_ORM:
					case MAMMA_ORM_UPLOAD_BEELDEN:
						responseMessageWrapper = hl7TestMessageService.verstuurORMTestBericht(hl7BerichtTekst.getObject());
						break;
					case MAMMA_ADT:
						responseMessageWrapper = hl7TestMessageService.verstuurADTTestBericht(hl7BerichtTekst.getObject());
						break;
					case MAMMA_ORM_VANUIT_IMS:
					case MAMMA_ORM_UPLOAD_BEELDEN_VANUIT_IMS:
						responseMessageWrapper = hl7TestMessageService.verstuurORMTestBerichtNaarScreenIT(hl7BerichtTekst.getObject());
						break;
					case MAMMA_ILM_VANUIT_IMS:
					case MAMMA_ILM_UPLOAD_BEELDEN_VANUIT_IMS:
						responseMessageWrapper = hl7TestMessageService.verstuurILMTestBerichtNaarScreenIT(hl7BerichtTekst.getObject());
						break;
					default:
						throw new IllegalStateException();
					}
					if (responseMessageWrapper != null)
					{
						info("Bericht verstuurd. Responsecode: " + responseMessageWrapper.getAcknowledgmentCodeString());
					}
					else
					{
						throw new IllegalStateException("Bericht is niet verzonden door een (verbindings)fout.");
					}
				}
				catch (Exception e)
				{
					LOG.error("Er is iets misgegaan met het versturen van een message", e);
					error(e.getMessage());
				}

			}
		});

	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(hl7BerichtTekst);
		ModelUtil.nullSafeDetach(testBerichtTypeModel);
	}
}
